module.exports = function (grunt) {
    'use strict';

    var log = grunt.log;
    var util = require('./mdbook-util')(grunt);

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
    grunt.registerMultiTask('md-book', 'a universal document converter', function () {
        var done = this.async();
        var files = this.data.files;
        var options = this.options({});

        grunt.verbose.writeflags(options, 'md-book options');

        if (files) {
            util(files, options, done);
        } else {
            invalidFiles(done);
        }
    });
};
