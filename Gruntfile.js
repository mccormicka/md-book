module.exports = function (grunt) {
    'use strict';

    var config = grunt.file.readJSON('package.json');
    var name = config.name || '';
    var title = config.title || '';
    var author = config.author || '';
    var license = config.license || '';
    var language = config.language || '';
    var cover = config.cover || 'Cover.png';
    var dest = config.dest || 'dist';
    var base = config.base || 'book';
    var styles = config.styles || 'styles';
    var chapters = config.chapters || base + '/chapters/*.md';
    var header = config.header || base + '/partials/head.html';
    var beforeBody = config.beforeBody || base + '/partials/beforeBody.html';
    var afterBody = config.afterBody || base + '/partials/afterBody.html';
    var epubMetadata = config.epubMetadata || base + '/partials/epub-metadata.xml';

    var compiledMarkdownFile = base + '/' + name + '.md';

    grunt.registerTask('default', ['readme', 'html', 'rtf', 'pdf', 'kindle']);
    grunt.registerTask('readme', ['shell:combine', 'shell:readme']);
    grunt.registerTask('html', ['shell:cleanHtml', 'shell:combine', 'pandoc:html', 'shell:moveMarkdown']);
    grunt.registerTask('epub', ['shell:combine', 'replace', 'pandoc:epub', 'shell:cleanEpub', 'shell:moveMarkdown']);
    grunt.registerTask('rtf', ['shell:combine', 'pandoc:rtf', 'shell:moveMarkdown']);
    grunt.registerTask('pdf', ['shell:combine', 'pandoc:pdf', 'shell:moveMarkdown']);
    grunt.registerTask('kindle', ['epub', 'pandoc:mobi']);

    // Project configuration.
    grunt.initConfig({

        //Compile all the chapters into one markdown file.
        shell: {
            combine: {
                command: 'awk \'{print}\' ' + chapters + ' > ' + compiledMarkdownFile
            },
            readme: {
                command: 'pandoc -f markdown -t markdown '+ compiledMarkdownFile+ ' -o README.md -s '
            },
            moveMarkdown: {
                command: 'mv ' + compiledMarkdownFile + ' ' + dest + '/'
            },
            cleanEpub: {
                command: 'rm ' + epubMetadata + '.temp'
            },
            cleanHtml: {
                command: ['rm ' + dest + '/' + styles + '/html.css', 'rm ' + dest + '/index.html'].join('&&')
            }
        },

        //Build epub metadata.
        replace: {
            files: {
                src: epubMetadata,
                dest: epubMetadata + '.temp'
            },
            options: {
                force: true,
                variables: {
                    'title': title,
                    'author': author,
                    'license': license,
                    'language': language
                }
            }
        },

        //Compile the one markdown file into seperate output files.
        //First Download and Install PanDoc
        //http://johnmacfarlane.net/pandoc/installing.html
        pandoc: {
            options: {
                dest: dest
            },
            html: {
                files: compiledMarkdownFile,
                options: {
                    '-o': 'index.html ',
                    '-s': '-t html5' +
                        ' --title-prefix "' + title + '" ' +
                        ' --include-in-header ' + header +
                        ' --include-before-body ' + beforeBody +
                        ' --include-after-body ' + afterBody +
                        ' --normalize ' +
                        ' --smart ' +
                        ' --toc ' +
                        ' --self-contained',
                    '-c': base + '/' + styles + '/html.css'
                }
            },
            epub: {
                files: compiledMarkdownFile,
                options: {
                    '-o': name + '.epub',
                    '-s': '-t epub ' +
                        ' --title-prefix "' + title + '" ' +
                        ' --epub-metadata ' + epubMetadata + '.temp' +
                        ' --epub-stylesheet ' + base + '/' + styles + '/epub.css' +
                        ' --epub-cover-image ' + base + '/' + cover +
                        ' --normalize ' +
                        ' --smart ' +
                        ' --toc '
                }
            },
            //Download: http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211
            //Symlink bin: ln -s /path/to/kindlegen /usr/local/bin
            mobi: {
                files: dest + '/' + name + '.epub',
                options: {
                    'kindle': true
                }
            },
            //You need `pdflatex`
            //OS X: http://www.tug.org/mactex/
            //Then find its path: find /usr/ -name "pdflatex"
            //Then symlink it: ln -s /path/to/pdflatex /usr/local/bin
            pdf: {
                files: compiledMarkdownFile,
                options: {
                    '-o': name + '.pdf',
                    '-s': '' +
                        ' --title-prefix "' + title + '" ' +
                        ' --epub-cover-image ' + base + '/' + cover +
                        ' --normalize ' +
                        ' --smart ' +
                        ' --toc '
                }
            },
            rtf: {
                files: compiledMarkdownFile,
                options: {
                    '-o': name + '.rtf',
                    '-s': '' +
                        ' --title-prefix "' + title + '" ' +
                        ' --normalize ' +
                        ' --smart ' +
                        ' --toc '
                }
            }
        }
    });

    //tasks for converting markdown files
    grunt.loadTasks('tasks/');
    grunt.loadNpmTasks('grunt-shell');
    grunt.loadNpmTasks('grunt-replace');
};
