const functions = require("firebase-functions");
const admin = require("firebase-admin");
const express = require("express");
const cors = require("cors");
const request = require("request-promise");
const { map } = require("lodash");
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
  console.log(config);
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

// Expose the API as a function
exports.api = functions.https.onRequest(app);
