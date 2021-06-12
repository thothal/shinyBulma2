// Stub is taken from: https://bit.ly/3x82MqW
/**
 * - yarn packages sit in node_modules
 * - we compile them to ../dist/lib/{package_name}
 * - mandatory css/js are also copied to the respective R place
 *   (i.e. ../dist/lib/bulma/bulma.min.css -> inst/lib/bulma/bulma.min.css)
 * - template css are rather big, hence the R package will provide a function to download
 *   them after installation (this allows smaller R package for a potential
 *   publictaion on CRAN)
 * - this install script will take the minified css from the github repo from folder dist/
 * - hence, we will despite good practices check in the dist folder
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
    dest_dist_dir: '../dist/lib/',
    dest_R_dir: '../inst/lib/',
  };

  grunt.initConfig({
    pkg: pkgInfo(),
    global_config: global_config,

    clean: {
      options: {
        force: true
      },
      all: {
        src: ['<%= clean.inst.src %>',
              '<%= clean.dist.src']
      },
      bulma: {
        src: ['<%= global_config.dest_dist_dir %>bulma/*.css',
              '!<%= global_config.dest_dist_dir %>bulma/*.min.css']
      },
      bulmaswatch: {
        src: ['<%= global_config.dest_dist_dir %>bulmaswatch/*.css',
              '!<%= global_config.dest_dist_dir %>bulmaswatch/*.min.css']
      },
      inst: {
        src: '<%= global_config.dest_R_dir %>/{bulma,bulmaswatch}/*'
      },
      dist: {
        src: '<%= global_config.dest_dist_dir %>/**/'
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
        dest:  '<%= global_config.dest_dist_dir %>bulma/',
        ext: '.css',
        flatten: true
      },
      bulmaswatch: {
        expand: true,
        src: ['node_modules/bulmaswatch/*/bulmaswatch.scss'],
        dest:  '<%= global_config.dest_dist_dir %>bulmaswatch/',
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

    copy: {
      bulma: {
        expand: true,
        src: '<%= cssmin.bulma.dest %>*.min.css',
        dest: '<%= global_config.dest_R_dir %>bulma',
        flatten: true
      },
      bulmaswatch: {
        expand: true,
        src: '<%= cssmin.bulmaswatch.dest %>*.min.css',
        dest: '<%= global_config.dest_R_dir %>bulmaswatch',
        flatten: true
      }
    },

    watch: {
      bulma: {
        files: '<%= sass.bulma.src %>',
        tasks: ['newer:sass:bulma',
                'newer:cssmin:bulma',
                'newer:clean:bulma',
                'newer:copy:bulma']
      },
      bulmaswatch: {
        files: '<%= sass.bulmaswatch.src %>',
        tasks: ['newer:sass:bulmaswatch',
                'newer:cssmin:bulmaswatch',
                'newer:clean:bulmaswatch']
      }
    }
  });

  grunt.registerTask('bulma',
    ['sass:bulma',
     'cssmin:bulma',
     'clean:bulma',
     'copy:bulma']);

  grunt.registerTask('bulmaswatch',
    ['sass:bulmaswatch',
     'cssmin:bulmaswatch',
     'clean:bulmaswatch']);

  // ---------------------------------------------------------------------------
  // Utility functions
  // ---------------------------------------------------------------------------

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
