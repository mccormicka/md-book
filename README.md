
Please Share on Twitter if you like #mdbook



<a href="https://twitter.com/intent/tweet?hashtags=mdbook&amp;&amp;text=Check%20out%20%23mdbook%20repo%20on%20github&amp;tw_p=tweetbutton&amp;url=https%3A%2F%2Fgithub.com%2Fdropshape&amp;via=dropshape" style="float:right"><img src="https://raw.github.com/dropshape/md-book/master/twittershare.png"></a>






%MarkDown to Book Readme
% by Anthony McCormick


# Description



mdbook wraps the [Pandoc](http://johnmacfarlane.net/pandoc) and [kindlegen](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000234621) CLI tools with a grunt task.

Any books that you can create using Pandoc you should be able to create using this task.



Please visit the [pandoc documentation](http://johnmacfarlane.net/pandoc/README.html) for full details



# Installation



## NPM

```

npm install md-book --save

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



# Usage



## Configuration

The Sample book ( the one your reading right now ) comes with a sample Gruntfile.js that shows how to create the following versions of your book.



 * MD

 * HTML

 * EPUB

 * RTF

 * PDF

 * KINDLE (mobi)



 The Gruntfile pulls its data from the package.json file for details about the books metadata. For example the book title, Author and language.



## Combining markdown files into one

The first step required in order to compile the book is combining of the individual markdown files into one. If you look at the example Gruntfile you will see a shell command that combines all the source markdown files into one markdown file with the title passed in the configuration file. You can run this combine task with the following command.



```

grunt shell:combine

```



## Running the sample project



Change directories to the mdbook folder and then run grunt to create various versions of this book in HTML, EPUB, MOBI etc..



```

cd node_modules/mdbook/

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







# Options



__Required__

mdbook supports all of pandocs options through it's own options object. However there is one required value that is not an option and that is the files property. This can be an array of files however it is best to combine all the markdown files into one file before passing it to this property as then it will create all the correct hyperlinks for you within your book. If you pass an array of markdown files then it will link to each markdown file and not the generated output.





```

pandoc :{

    files: compiledMarkdownFile

}

```



__Optional__



**dest** Location where you would like the book output. If the folder does not already exist it will be created. If are outputting html then you should consider using the ``` --self-contained ``` flag to force inclusion of your linked assets



**-o** : Output file name for example index.html or Title.pdf.



**-c** : CSS file to associate with the document for example html.css or epub.css



**-s** : All other pandoc options can be referenced through the command options for example you can set -t epub to make your book an epub book or you can set --toc to create a table of contents please refer to the options from the [pandoc documentation](http://johnmacfarlane.net/pandoc/README.html).



**kindle** : If set to true then you must pass an epub file as the files parameter. This task will then output a kindle mobi file generated using [kindlegen](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000234621)





## Including scripts



MD2Book includes a haskell script that will allow you to link in your code rather than copy pasting it into the MD files. Anywhere from within your .md files you can add the following code which will place the included file into a code block with the style set to whatever style you wish to have applied.



```

~~~~ {include="tasks/mdbook.js" style="javascript"}

~~~~

```



__required__



__include__ : Path to the file to include.



__optional__



__style__ : Style of the code



This will produce for example:



~~~~ {include="tasks/mdbook.js" style="javascript"}

~~~~







# CHANGELOG



## 0.0.3



Automatically load the include filter so that GruntFiles do not need to be updated.

See [Including scripts] for details.



## 0.0.2

Added the ability to include external files so that code can be linked to rather than copy pasted into the book.

See [Including scripts] for details.



## 0.0.1

Initial Release

Please Share on Twitter if you like #mdbook



<a href="https://twitter.com/intent/tweet?hashtags=mdbook&amp;&amp;text=Check%20out%20%23mdbook%20repo%20on%20github&amp;tw_p=tweetbutton&amp;url=https%3A%2F%2Fgithub.com%2Fdropshape&amp;via=dropshape" style="float:right"><img src="https://raw.github.com/dropshape/md-book/master/twittershare.png"></a>






