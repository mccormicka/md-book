module.exports = function (grunt) {
    'use strict';
    var log = grunt.log;
    var exec = (require('child_process')).exec;
    var fs = require('fs');
    var glob = require('glob');

    //-------------------------------------------------------------------------
    //
    // Private Methods
    //
    //-------------------------------------------------------------------------

    function pandocCompileFile(file, options, done) {
        var baseDirectory = process.cwd();
        invokeCommand(applyPandocOptions(options, file), callbackObject(done));
        process.chdir(baseDirectory);
    }

    function applyPandocOptions(options, file) {
        var key;
        var value;
        var commandString = 'pandoc ' + file;
        buildOutputDirectory(options);
        if(options.kindle){
            log.writeln('Called kindlegen');
            commandString = 'kindlegen ' + file;
            return commandString;
        }
        for (key in options) {
            value = options[key];
            commandString += ' ' + key;
            if (value !== null) {
                commandString += ' ' + value;
            }
        }
        console.log('Running command : ', commandString);
        return commandString;
    }

    function buildOutputDirectory(options) {
        if (options.dest) {
            createDirectoryIfItDoesNotExist(options.dest);
            //Repackage to destination folder
            var file = options['-o'];
            options['-o'] = options.dest + '/' + file;
//            var stylesheet = options['-c'];
//            if (stylesheet) {
//                var styles = glob.sync('**/**/' + stylesheet);
//                log.writeln('Style is ', styles);
//                if (styles.length > 0) {
//                    var style = styles[0];
//                    var stylesheetDest = options.dest + '/' + stylesheet;
//                    createFileDirectoryIfItDoesNotExist(stylesheetDest);
//                    copyFile(style, stylesheetDest, function (err) {
//                        if (err) {
//                            log.writeln('Error copying stylesheet', err);
//                        }
//                    });
//                }
//            }
            delete options.dest;
        }
    }

    function createFileDirectoryIfItDoesNotExist(filePath) {
        var index = filePath.lastIndexOf('/');
        if (index > -1) {
            filePath = filePath.slice(0, index);
        }
        createDirectoryIfItDoesNotExist(filePath);
    }

    function createDirectoryIfItDoesNotExist(directory) {
        if (!fs.existsSync(directory)) {
            log.writeln('path does not exist');
            fs.mkdirSync(directory);
        }
    }

    function invokeCommand(cmd, callbacks) {
        var process = exec(cmd);
        process.stdout.on('data', callbacks.onstdout);
        process.stderr.on('data', callbacks.onstderr);
        process.on('exit', function (code) {
            code === 0 ? callbacks.onsuccess() : callbacks.onfail();
        });
    }

    function callbackObject(done) {
        return {
            onstdout: function (output) {
                return log.writeln(output);
            },
            onstderr: function (output) {
                log.writeln(output);
                return grunt.event.emit('pandoc.error', output);
            },
            onsuccess: function () {
                log.writeln('compiled document');
                return done(true);
            },
            onfail: function () {
                log.writeln('failed compiling document');
                return done(false);
            }
        };
    }

    //Taken from http://stackoverflow.com/questions/11293857/fastest-way-to-copy-file-in-node-js
    function copyFile(source, target, cb) {
        var cbCalled = false;

        var rd = fs.createReadStream(source);
        rd.on('error', function (err) {
            done(err);
        });
        var wr = fs.createWriteStream(target);
        wr.on('error', function (err) {
            done(err);
        });
        wr.on('close', function () {
            done();
        });
        rd.pipe(wr);

        function done(err) {
            if (!cbCalled) {
                cb(err);
                cbCalled = true;
            }
        }
    }

    //-------------------------------------------------------------------------
    //
    // Public API
    //
    //-------------------------------------------------------------------------

    return function compileDocument(files, options, done) {
        grunt.file.expand({
            filter: 'isFile'
        }, files).forEach(function (file) {
                pandocCompileFile(file, options, done);
            });
    };
};

