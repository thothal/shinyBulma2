// Stub is taken from: https://bit.ly/3x82MqW

module.exports = function(grunt) {

  const src_dir_js = '../srcjs/';
  const src_dir_css = '../inst/';
  const dest_dir_js = '../inst/';
  const dest_dir_css = '../inst/';

  grunt.initConfig({
    pkg: pkgInfo(),

    clean: {
      options: { force: true },
      src: [dest_dir_js + '**/*.{js,map}',
            dest_dir_css + '**/*.css',
            dest_dir_css + '{bulma,bulmaswatch}/'
      ]
    },

    copy: {
      bulma: {
        expand: true,
        src: ['node_modules/bulma/css/bulma.min.css',
              'node_modules/bulma/css/bulma.css.map'],
        dest: dest_dir_css + 'bulma/',
        flatten: true
      },
      bulmaswatch: {
        expand: true,
        src: ['node_modules/bulmaswatch/*/*.min.css',
              'node_modules/bulmaswatch/*/*.css.map'],
        dest: dest_dir_css + 'bulmaswatch/',
        rename: function(dest, src) {
          const path = require('path');
          const folder_name = path.basename(path.dirname(src));
          const file_extension = path.basename(src).replace(/.+(\.min\.css(\.map)?)/, '$1');
          const file = dest + folder_name + file_extension;
          return file;
        }
      }
    },

    watch: {
      bulma: {
        files: '<%= copy.bulma.src %>',
        tasks: ['newer:copy:bulma']
      },
      bulmaswatch: {
        files: '<%= copy.bulmaswatch.src %>',
        tasks: ['newer:copy:bulmaswatch']
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-newer');

  //grunt.registerTask('default', ['newer:concat', 'newer:eslint', 'newer:uglify', 'newer:cssmin']);


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
