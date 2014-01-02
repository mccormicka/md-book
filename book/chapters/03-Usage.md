#Usage

## Configuration
The Sample book ( the one your reading right now ) comes with a sample Gruntfile.js that shows how to create the following
versions of your book.

 * MD
 * HTML
 * EPUB
 * RTF
 * PDF
 * KINDLE (mobi)

 The Gruntfile pulls its data from the package.json file for details about the books metadata. For example the book title,
 Author and language.

## Combining markdown files into one
The first step required in order to compile the book is combining of the individual markdown files into one. If you look
at the example Gruntfile you will see a shell command that combines all the source markdown files into one markdown file
with the title passed in the configuration file. You can run this combine task with the following command.

```
grunt shell:combine
```

## Running the sample project

Change directories to the md2book folder and then run grunt to create various versions of this book in HTML, EPUB, MOBI etc..

```
cd node_modules/md2book/
grunt
```
or build only one type of book with one of the following commands.

```
grunt readme
grunt html
grunt epub
grunt rtf
grunt pdf
grunt kindle
```