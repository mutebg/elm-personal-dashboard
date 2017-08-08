const functions = require("firebase-functions");
const admin = require("firebase-admin");
const express = require("express");
const cors = require("cors");
const request = require("request-promise");
const { map, get } = require("lodash");
const Twitter = require("twitter");

const localConfig = require("./config.json");

admin.initializeApp(functions.config().firebase);

const app = express();
app.use(cors());

const config = functions.config().github ? functions.config() : localConfig;

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
      response.map(repo => ({
        title: repo.full_name,
        url: repo.html_url
      }))
    );
  });
});

app.get("/twitter/:user", (req, res) => {
  const client = new Twitter({
    consumer_key: config.twitter.key,
    consumer_secret: config.twitter.secret,
    access_token_key: "",
    access_token_secret: ""
  });

  const user = req.params.user;

  client.get(
    "statuses/user_timeline",
    { screen_name: user, count: 10 },
    (error, tweets, response) => {
      res.status(201).json(
        tweets.map(t => ({
          title: t.text,
          url: `https://twitter.com/${user}/status/${t.id_str}`
        }))
      );
    }
  );
});

app.get("/lastfm/:user/:type", (req, res) => {
  const url = `http://ws.audioscrobbler.com/2.0/?method=user.${req.params
    .type}&user=${req.params.user}&api_key=${config.lastfm.apikey}&format=json`;

  let key = [];
  let fn = track => ({
    title: track.artist["#text"] + " - " + track.name,
    url: track.url
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
    res.status(201).json(get(response, key).map(fn));
  });
});

// Expose the API as a function
exports.api = functions.https.onRequest(app);