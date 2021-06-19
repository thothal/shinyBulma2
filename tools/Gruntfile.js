// Stub is taken from: https://bit.ly/3x82MqW
/**
 * - yarn packages sit in node_modules
 * - we compile them to ../inst/lib/{package_name}
 * - template css are rather big, hence the R package will provide a function to download
 *   them after installation (this allows smaller R package for a potential
 *   publictaion on CRAN)
 * - this install script will take the minified css from the github repo from folder inst/
 * - the .Rbuildignore file excludes all files from the theme folder from building
 * - homebrewed css/js will also be handled by this grunt file, for R reasons we cannot
 *   use the canonical folder name src, but use srcjs/ and srccss/ instead
 **/

module.exports = function(grunt) {
  require('load-grunt-tasks')(grunt);
  require('time-grunt')(grunt);

  const sass = require('sass');

  const global_config = {
    src_dir_js: '../srcjs/',
    src_dir_css: '../srccss/',
    dest_dir_lib: '../inst/lib/',
    dest_dir_fonts: '../inst/fonts/'
  };



  grunt.initConfig({
    pkg: pkgInfo(),
    global_config: global_config,

    patch: {

    },

    clean: {
      options: {
        force: true
      },
      all: {
        src: ['<%= clean.libs.src %>',
              '<%= clean.fonts.src %>']
      },
      bulma: {
        src: ['<%= global_config.dest_dir_lib %>bulma/*.css',
              '!<%= global_config.dest_dir_lib %>bulma/*.min.css']
      },
      bulmaswatch: {
        src: ['<%= global_config.dest_dir_lib %>bulmaswatch/*.css',
              '!<%= global_config.dest_dir_lib %>bulmaswatch/*.min.css']
      },
      fonts: {
        src: ['<%= global_config.dest_dir_fonts %>*']
      },
      libs: {
        src: '<%= global_config.dest_dir_lib %>{bulma,bulmaswatch}/*'
      }
    },

    sass: {
      options: {
        implementation: sass,
        sourceMap: false,
        includePaths: ['node_modules/bulma']
      },
      bulma: {
        expand: true,
        src: ['node_modules/bulma/bulma.sass'],
        dest:  '<%= global_config.dest_dir_lib %>bulma/',
        ext: '.css',
        flatten: true
      },
      bulmaswatch: {
        expand: true,
        src: ['node_modules/bulmaswatch/*/bulmaswatch.scss',
              '!node_modules/bulmaswatch/default/bulmaswatch.scss'],
        dest:  '<%= global_config.dest_dir_lib %>bulmaswatch/',
        /**
         * Files in bulmaswatch are all named bulmaswatch.scss rename to basefolder
         **/
        rename: function(dest, src) {
          const path = require('path');
          const folder_name = path.basename(path.dirname(src));
          const file = dest + folder_name + '.css';
          return file;
        }
      }
    },

    run: {
      fonts: {
        cmd: 'Rscript',
        args: ['change_gfont_to_local.R',
               '<%= grunt.file.expand([' +
                     'grunt.config.get("sass.bulmaswatch.dest") + "/*.css", ' +
                     '"!" + grunt.config.get("sass.bulmaswatch.dest") + "/*.min.css"' +
               ']) %>']
      }
    },

    cssmin: {
      bulma: {
        expand: true,
        cwd: '<%= sass.bulma.dest %>',
        src: ['*.css', '!*.min.css'],
        dest: '<%= sass.bulma.dest %>',
        ext: '.min.css',
        flatten: true
      },
      bulmaswatch: {
        expand: true,
        cwd: '<%= sass.bulmaswatch.dest %>',
        src: ['*.css', '!*.min.css'],
        dest: '<%= sass.bulmaswatch.dest %>',
        ext: '.min.css',
        flatten: true
      }
    },

    watch: {
      bulma: {
        files: '<%= sass.bulma.src %>',
        tasks: ['newer:sass:bulma',
                'newer:cssmin:bulma',
                'newer:clean:bulma']
      },
      bulmaswatch: {
        files: '<%= sass.bulmaswatch.src %>',
        tasks: ['newer:sass:bulmaswatch',
                'newer:run:fonts',
                'newer:cssmin:bulmaswatch',
                'newer:clean:bulmaswatch']
      }
    }
  });

  grunt.registerTask('bulma',
    ['sass:bulma',
     'cssmin:bulma',
     'clean:bulma']);

  grunt.registerTask('bulmaswatch',
    ['sass:bulmaswatch',
     'run:fonts',
     'cssmin:bulmaswatch',
     'clean:bulmaswatch']);

  grunt.registerTask('default', ['bulma', 'bulmaswatch']);

  grunt.registerMultiTask('foo', 'TL;DR', function() {
    grunt.log.writeln(this.filesSrc);
  });
  // ---------------------------------------------------------------------------
  // Utility functions
  // ---------------------------------------------------------------------------


  function getCSSFromPattern(src_dest) {

  }

  // Return an object which merges information from package.json and the
  // DESCRIPTION file.
  function pkgInfo() {
    var pkg = grunt.file.readJSON('package.json');

    pkg.name    = descKeyValue('Package');
    pkg.version = descKeyValue('Version');
    pkg.license = descKeyValue('License');

    return pkg;
  }

  // From the DESCRIPTION file, get the value of a key. This presently only
  // works if the value is on one line, the same line as the key.
  function descKeyValue(key) {
    var lines = require('fs').readFileSync('../DESCRIPTION', 'utf8').split('\n');

    var pattern = new RegExp('^' + key + ':');
    var txt = lines.filter(function(line) {
      return pattern.test(line);
    });

    txt = txt[0];

    pattern = new RegExp(key + ': *');
    txt = txt.replace(pattern, '');

    return txt;
  }
};
