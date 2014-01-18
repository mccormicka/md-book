#Installation

##NPM
```
npm install mdbook --save
```

## Other Dependencies

### [Pandoc](http://johnmacfarlane.net/pandoc)
[Pandoc](http://johnmacfarlane.net/pandoc) is the only required CLI tool however if you want to create PDF or KINDLE(mobi) files then you need to install extra tools outlined below.

### [PDFLATEX](http://www.tug.org/mactex/)
In order to build out PDF files you need to install [MACTEX](http://www.tug.org/mactex/) so that you can use [PDFLATEX](http://www.tug.org/mactex/) this is a rather huge download of more than 2.gig once downloaded unpack and run the following commands

```
find /usr/ -name "pdflatex"
ln -s /path/to/pdflatex /usr/local/bin
```

### [KINDLEGEN](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211)
In order to build mobi files that can be used on kindle devices you need to download [KINDLEGEN](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211) once downloaded unpack it and run the following command.

```
ln -s /path/to/kindlegen /usr/local/bin
```

You can now build all the available types mentioned in the [**Usage guide**](#usage).

