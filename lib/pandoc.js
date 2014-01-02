module.exports = function (grunt) {
    'use strict';

    var log = grunt.log;
    var task = require('./pandoc-task')(grunt);

    function invalidFiles(done) {
        var error = 'No files property found!';
        log.writeln(error);
        grunt.event.emit('pandoc.error', error);
        done(false);
    }

    /**
     * Grunt Task.
     * @see README.md
     */
    return grunt.registerMultiTask('pandoc', 'a universal document converter', function () {
        var done = this.async();
        var files = this.data.files;
        var options = this.options({});

        grunt.verbose.writeflags(options, 'Pandoc options');

        if (files) {
            task(files, options, done);
        } else {
            invalidFiles(done);
        }
    });
};
