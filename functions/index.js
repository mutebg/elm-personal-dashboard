const functions = require("firebase-functions");
const admin = require("firebase-admin");
const express = require("express");
const cors = require("cors");
const request = require("request-promise");
const { map, get, round } = require("lodash");
const Twitter = require("twitter");
const strava = require("strava-v3");

const localConfig = require("./config.json");

admin.initializeApp(functions.config().firebase);

const app = express();
app.use(cors());
app.use((req, res, next) => {
  res.set("Cache-Control", "public, max-age=3000, s-maxage=6000");
  next();
});

//const config = functions.config().github ? functions.config() : localConfig;
const config = localConfig;

const mediumData = (user, type) =>
  request({
    uri: `https://medium.com/@${user}/${type}?format=json`,
    json: true
  }).then(body => {
    const jsonBody = JSON.parse(body.replace("])}while(1);</x>", ""));
    return map(jsonBody.payload.references.Post, ({ title, uniqueSlug }) => ({
      title,
      url: uniqueSlug
    }));
  });

app.get("/medium/:user/:type", (req, res) => {
  mediumData(req.params.user, req.params.type).then(posts =>
    res.status(201).json(posts)
  );
});

app.get("/github/:user/:type", (req, res) => {
  const token = config.github.token;
  request({
    json: true,
    headers: {
      "User-Agent": "Request-Promise"
    },
    uri: `https://api.github.com/users/${req.params
      .user}/repos?sort=pushed&access_token=${token}`
  }).then(response => {
    res.status(201).json(
      response
        .map(repo => ({
          title: repo.full_name,
          url: repo.html_url
        }))
        .slice(0, 10)
    );
  });
});

app.get("/twitter/:user/:type", (req, res) => {
  const client = new Twitter({
    consumer_key: config.twitter.key,
    consumer_secret: config.twitter.secret,
    access_token_key: "",
    access_token_secret: ""
  });

  const user = req.params.user;

  const type =
    req.params.type == "favorites"
      ? "favorites/list"
      : "statuses/user_timeline";

  client.get(
    type,
    { screen_name: user, count: 10 },
    (error, tweets, response) => {
      res.status(201).json(
        tweets
          .map(t => ({
            title: t.text,
            url: `https://twitter.com/${t.user.screen_name}/status/${t.id_str}`,
            image_url: t.user.profile_image_url
          }))
          .slice(0, 10)
      );
    }
  );
});

app.get("/lastfm/:user/:type", (req, res) => {
  const url = `http://ws.audioscrobbler.com/2.0/?method=user.${req.params
    .type}&user=${req.params.user}&api_key=${config.lastfm.apikey}&format=json`;

  let key = [];
  let fn = track => ({
    title: track.artist["#text"],
    sub: track.name,
    url: track.url,
    image_url: get(track, ["image", "0", "#text"])
  });
  switch (req.params.type) {
    case "getRecentTracks":
      key = ["recenttracks", "track"];
      break;
    case "getWeeklyTrackChart":
      key = ["weeklytrackchart", "track"];
      break;
    case "getWeeklyAlbumChart":
      key = ["weeklyalbumchart", "album"];
      fn = album => ({
        title: album.name + " by " + album.artist["#text"],
        url: album.url
      });
      break;
    case "getWeeklyArtistChart":
      key = ["weeklyartistchart", "artist"];
      fn = artist => ({
        title: artist.name,
        url: artist.url
      });
      break;
  }

  const token = config.github.token;
  request({
    json: true,
    headers: {
      "User-Agent": "Request-Promise"
    },
    uri: url
  }).then(response => {
    res.status(201).json(get(response, key).map(fn).slice(0, 10));
  });
});

app.get("/instagram/:user/:type", (req, res) => {
  request({
    json: true,
    uri: `https://igpi.ga/${req.params.user}/media/?count=10`
  }).then(response => {
    res.status(201).json(
      response.items.map(ig => ({
        title: ig.caption ? ig.caption.text : "",
        url: ig.link,
        image_url: ig.images.low_resolution.url
      }))
    );
  });
});

app.get("/setlistfm/:user", (req, res) => {
  request({
    json: true,
    uri: `https://api.setlist.fm/rest/1.0/user/${req.params.user}/attended`,
    headers: {
      Accept: "application/json",
      "x-api-key": config.setlistfm.apikey
    }
  }).then(response => {
    res.status(201).json(
      response.setlist.slice(10).map(i => ({
        title: i.artist.name + " @ " + i.venue.name + " / " + i.eventDate,
        url: i.url
      }))
    );
  });
});

app.get("/strava/activities", (req, res) => {
  strava.athlete.listActivities(
    { access_token: config.strava.access_token, per_page: 10 },
    (err, payload, limits) => {
      if (!err) {
        res.status(201).json(
          payload.map(item => {
            const date = new Date(null);
            date.setSeconds(item.moving_time); // specify value for SECONDS here
            const distance = round(item.distance / 1000, 2);
            const time = date.toISOString().substr(11, 8);
            const speed = round(item.average_speed * 3.6, 2);
            return {
              title: item.name,
              sub: `distance: ${distance} km | time: ${time} | speed: ${speed} km/h`,
              url: `https://www.strava.com/activities/${item.id}`,
              image_url: `https://d2u2bkuhdva5j0.cloudfront.net/activities/${item.id}/a/2/300x300.png`
            };
          })
        );
      } else {
        console.log(err);
      }
    }
  );
});

app.get("/strava/stats", (req, res) => {
  strava.athletes.stats(
    { id: 8171123, access_token: config.strava.access_token },
    (err, payload, limits) => {
      res.status(201).json(payload);
    }
  );
});

app.get("/rescuetime/daily", (req, res) => {
  const keys = [
    "communication_and_scheduling",
    "social_networking",
    "design_and_composition",
    "entertainment",
    "news",
    "software_development",
    "reference_and_learning",
    "utilities"
  ];

  const url =
    "https://www.rescuetime.com/anapi/daily_summary_feed?format=json&key=" +
    config.rescuetime.key;
  request({
    json: true,
    uri: url
  }).then(response => {
    res.status(201).json(
      response.map(item => {
        const max = keys.reduce((prev, current) => {
          if (item[current + "_hours"] > prev) {
            return item[current + "_hours"];
          }
          return prev;
        }, 0);
        const onePercent = 100 / max;
        const data = keys.map(key => ({
          label: key.replace(/_/g, " "),
          value: Math.round(item[key + "_hours"] * onePercent)
        }));

        return {
          title: item.date,
          url: "",
          sub: item.total_duration_formatted,
          data
        };
      })
    );
  });
});

// Expose the API as a function
exports.api = functions.https.onRequest(app);
