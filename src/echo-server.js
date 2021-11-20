const http = require("http");

const hostname = "0.0.0.0";
const port = 3000;

const server = http.createServer((req, res) => {
  console.log(`\n${req.method} ${req.url}`);
  console.log(req.headers);

  const reverseArray = a => {
    var na = [];
    for (var i = 0; i < a.length; ++i) {
      const value = a[i];
      const nValue = value.constructor.name == 'Object'
        ? reverseKeys(value) : value;
      na.push(nValue);
    }
    return na;
  }
  const reverseKeys = o => {
    const no = {};
    for (const [key, value] of Object.entries(o)) {
      const nValue = value.constructor.name == 'Object'
        ? reverseKeys(value)
        : (value.constructor.name == 'Array'
          ? reverseArray(value) : value);
      no[key.split("").reverse().join("")] = nValue;
    }
    return no;
  };

  req.on("data", function (chunk) {
    console.log("BODY: " + chunk);
    res.statusCode = 200;
    res.setHeader("Content-Type", "text/plain");
    const o = reverseKeys(JSON.parse(chunk));
    const response = JSON.stringify(o, null, 2);
    console.log("RESPONSE: " + response);
    res.end(response);
  });

});

server.listen(port, hostname, () => {
  console.log(`Server running at http://localhost:${port}/`);
});