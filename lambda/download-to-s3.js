const AWS = require('aws-sdk'),
      stream = require('stream'),
      https = require('https');

exports.handler = (req, context, callback) => {
    console.log("req:", req);
    console.log("Fetching", req.src);

    https.get(req.src, res => {
        const passthrough = new stream.PassThrough();
        if (res.statusCode != 200) {
            callback ("Error: " + res.statusCode);
            return;
        }

        const upload = new AWS.S3.ManagedUpload({
            params: {
                Bucket: req.dest.bucket,
                Key: req.dest.key,
                Body: passthrough
            },
            partSize: 1024 * 1024 * 8,
        });

        upload.send(err => {
            if (err) {
                callback(err);
            } else {
                callback(null, {statusCode: 200, body: JSON.stringify('Done!')});
            }
        });

        res.pipe(passthrough);
    });
};
