# gopro cloud management tools

I like my GoPros and have been using GoPro cloud since around the end
of 2019.  I wrote [a blog post][blog] about it in more detail, but in
summary, GoPro Plus isn't sufficiently useful to me without some
additional tools.  This package provides those tools.

## Getting Started

Prerequisites:

In order to process streams out of video files, you'll need
[ffmpeg][ffmpeg] (most likely available in your system package
manager) somewhere on the path.

You'll also need [stack][stack] to build and install the software.

### Installation

Install the tool (which requires [stack][stack]):

    git clone https://github.com/dustin/gopro
    cd gopro
    stack install

### First Run

Now you've got the `gopro` tool available.  Everything the tool does
requires a database, which is `gopro.db` in the directory you run it
(i.e., you can have multiple if you just run it from different
locations).

Pick a spot and authenticate:

    mkdir ~/gopro
    cd ~/gopro
    gopro auth

This will prompt you for your GoPro Plus username and password.  Once
that's done, you're ready to go!

### Claim Your Data

At this point, you can either use the commandline tools, e.g. `gopro
sync` to pull down all the GoPro metadata into your local database, or
you can run `gopro serve` and do it all via the web interface
(assuming you tell it where its static content is).

## Commandline Reference

All tools take the following arguments:

```
  --dbpath ARG             db path (default: "gopro.db")
  --static ARG             static asset path (default: "static")
  -v,--verbose             enable debug logging
  -u,--upload-concurrency ARG
                           Upload concurrency (default: 3)
  -d,--download-concurrency ARG
                           Download concurrency (default: 11)
```

Most of them should be obvious, but `--static` is the location of the
web media, which you may need to provide if you're running it
somewhere in particular.

### auth

The `auth` command, as mentioned in the "Getting Started" section
authenticates you with the service.  This authentication token lasts
around a day or less.

You should generally only ever need to do this once per `gopro.db`.
If you have multiple installations, each one will need to be
authenticated.

### reauth

The `reauth` command will use a stored token to refresh your
credentials.  You'll need to do this periodically.  You do not need to
supply your password again (i.e., you can just run this on a timer or
something if you plan to mostly interact with the web service).

This feature is available via the web interface by clicking on "🔒".

### sync

The `sync` command finds all of your recently uploaded media that the
local database does not have yet, and grabs that data.

It also extracts all the metadata and gives you some nice rich local
data features.

This feature is available via the web interface by clicking on "🔃".

### refresh

The `refresh` command updates the local metadata for specific
`MediumID`s.  e.g., if you've made changes to highlights or similar on
the GoPro site, you'd use this to retrieve the latest changes.

This feature is available via the web interface when looking at
specific item details.

### upload

The `upload` command uploads media from your computer.  This is
similar functionality to what your camera might do, but doing it from
your computer gives you a bit more control.

`upload` in particular does two distinct things:

1. It creates GoPro-side media (tells them there's something coming
   and gets IDs) and associates those media with local files.
2. It gets all the bits of those files uploaded and marks things done.

These two things are technically decoupled, but from a UX point of
view, you can start an upload, interrupt it, and then start it again
later and it will continue from where you left off.

Technically, if all you want to do is resume an upload, you can just
type `gopro upload` and it will finish any that it knows about, but
are not done.

See also: `createupload`, `createmulti`

### serve

The `serve` command runs a web server and lets you browse and search
your media quickly and easily.

## Less Common Commands

### createupload

The `createupload` command only executes the first half of the
`upload` sequence.  This is useful when you want to define a bunch of
uploads for local files and then execute them later.  It's also useful
when you have multi-part uploads you want to mix in with the upload
batch and want to just prepare everything first.

### createmulti

The `createmulti` command creates uploads that span multiple files.
There are two main (and a few minor) use cases for this:

1. Video recordings that exceed individual file length and were split
   into multiple files.
2. Timelapse photos (as opposed to video) where a single button press
   spat out a large number of image files.

While this is less commonly used (at least for me), it's been
extremely important functionality that isn't available anywhere other
than the camera itself, so it's helped me with ~thousand file time
lapses I took with my Hero 4 back in the day, for example.

Usage is a bit tedious, but it's not too bad.  First, you have to
consider the type of media you're intending to upload.  It must be one
of the following (exact, case sensitive):

* Photo
* Video
* TimeLapse
* TimeLapseVideo
* Burst

Then, you just run the command with the media you want to upload, in
order (usually alphabetical, so `*` tends to work).  e.g.:

    gopro createmulti TimeLapse *.JPG

This will define an upload that includes all matching JPGs that will
be considered a `TimeLapse`.

After you define your multipart uploads, you can use the regular `gopro
upload` command to complete them.

### fetchall

The `fetchall` command is essentially `sync`, but without stopping
when it sees something it's seen before.  It will take longer than
`sync` but should generally do the same thing unless you've rampage
deleted some data from `gopro.db`.

### cleanup

The `cleanup` command cleans data on the GoPro side.  If you've ever
tried to upload things from the web UI and had it tell you the media's
already been upload (when it hasn't), or if you started an upload you
don't intend to finish and want to get rid of in-progress stuff, the
`cleanup` command will delete all of these in-progress things.

### config

The `config` command lets you view and update configuration
parameters.  It's got three modes of execution.

To list all configuration parameters and their values:

    gopro config

To display the value of the configuration parameter `bucket`:

    gopro config bucket

To set the value of the configuration `parameter` to
`some.bucket.name`:

    gopro config bucket some.bucket.name

### wait

The `wait` command just waits for in-progress uploads and (GoPro-side)
transcoding to finish.  `sync` does this automatically, but if you
want to wait for some other reason without syncing, this will do it.

### fixup

The fixup command allows you to write a SQL query against your local
metadata database that will update fields in GoPro's service for any
matching rows.

This is a fairly advanced and potentially terribly destructive thing
to do.  Everything possible to do here is well beyond the scope of a
README, but at a high level, you write a query that returns rows whose
column names must include `media_id` for the item you wish to update,
and then additional columns whose names match metadata field names in
GoPro metadata.  By the time you get to the point where you're doing
this, you probably know all this.

As an example, when uploading from some source (maybe my phone?) or in
some fashion I don't remember, GoPro seemed to not know which camera
various media camera from.  But the local metadata knew because it
read it directly from the GPMF data.  I wanted GoPro's metadata to be
consistent, so I wrote the following query:

```sql
select m.media_id, g.camera_model
    from media m join meta g on (m.media_id = g.media_id)
    where m.camera_model is null
    and g.camera_model is not null
```

This query spat out every row where GoPro didn't know the camera
model, but I was able to derive it from metadata.  I fed that to the
`fixup` command and all my metadata on their end was happily updated.
(I'd still need to use `refresh` to update the local copies).

### backup

The `backup` command will orchestrate a move of all original media
stored in GoPro to your own S3 bucket.  Lots of stuff is involved in
setup here including that S3 bucket, an AWS Lambda copy function and
an SQS queue.

This does work, and should be able to move a huge amount of data in a
short amount of time.  The tl;dr on how to use is:

1. Create an S3 bucket in `us-west-2` (Oregon) to store all your
   stuff.  Note that this is used for both metadata cache and backups.
2. Create a lambda function (I called it `download-to-s3`, but it's
   configurable) in `us-west-2` as a node.js runtime with `index.js`
   containing the contents from the file
   [lambda/download-to-s3.js](lambda/download-to-s3.js)
3. Set up an SQS queue, also in `us-west-2` to capture the results.
4. Configure destinations for both success and failure to this new SQS
   queue.
5. Configure (using `gopro config`) `s3copySQSQueue` to point to your
   new SQS queue, `bucket` to point to your S3 bucket, and
   `s3copyfunc` config variable is pointed to the correct function
   name (i.e., change it if it's not `download-to-s3`).

With this AWS-side infrastructure in place, `gopro backup` should copy
all the things.

This primarily exists as a proof of concept that I hope I won't ever
need, as I don't mind using GoPro's storage (it's S3) and paying for
the whole thing to be reproduced in my own storage kind of makes the
service less useful.

When I do, I suspect I should be able to move my >TB storage from
GoPro's buckets to my own with a tiny amount of bandwidth to my house.

### backupall

The `backupall` command is similar to the `backup` command, but grabs
all known media stored on your behalf, including derivatives.

### backuplocal

The `backuplocal` command is similar to the `backup` command, except
it copies data locally instead of to an S3 bucket (and runs entirely
locally).

Given an argument for the destination path, it will attempt to
download all original artifacts for a given medium and once complete,
will move the destination directory into place.  Once a directory for
a given medium is in place, it will not attempt to download the same
medium again (i.e., if you delete the directory for a medium, it will
be redownloaded).

### backuplocalall

The `backuplocalall` command is similar to the `backuplocal` commnand,
except it fetches all known media instead of just originals.

### processSQS

The `processSQS` command is automatically run by the `backup` command
to catch results of asynchronous lambda function calls, but if the
process is interrupted or you just want to make sure you've picked up
everything, this command can be invoked separately without potentially
issuing more copy requests.

[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[blog]: http://dustin.sallings.org/2020/04/29/gopro-plus.html
