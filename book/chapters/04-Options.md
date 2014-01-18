#Options

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



