// @@ make sure that websocketd is installed
// @@ make sure that ./dr-ws.sh is running

var WebSocketClient = require('websocket').client;
var client = new WebSocketClient();

client.on('connectFailed', function(error) {
    console.log('Connect Error: ' + error.toString());
});

client.on('connect', function(connection) {
    connection.on('error', function(error) {
        console.log("Connection Error: " + error.toString());
    });
    connection.on('message', function(message) {
        if (message.type === 'utf8') {
            console.log(message.utf8Data);
        }
    });
    
    connection.send("https://w3c.github.io/N3/files/state%20transitions/socrates/socrates-query1.n3");
    connection.send("https://w3c.github.io/N3/files/state%20transitions/socrates/socrates-query2.n3");
    connection.send("https://w3c.github.io/N3/files/state%20transitions/socrates/socrates-query1.n3");
    connection.send("end_of_file");
});

client.connect('ws://localhost:8000/');
