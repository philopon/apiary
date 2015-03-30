'use strict';

var gulp    = require('gulp');
var webpack = require('gulp-webpack');
var rename  = require('gulp-rename');
var jade    = require('gulp-jade');
var connect = require('gulp-connect');
var sass    = require('gulp-sass');
var plumber = require('gulp-plumber');
var uglify  = require('gulp-uglify');
var run     = require('gulp-run');

var postcss  = require('gulp-postcss');
var prefixer = require('autoprefixer-core');
var mqpacker = require('css-mqpacker');
var csswring = require('csswring');

var webpackLib = require('webpack');

var del = require('del');

var path = require('path');
var developDir = "develop";
var tmpDir = "tmp";
var productionDir = "dist";

function html(develop, src, dest){
  return function(){
    var s = gulp.src(src);
    if(develop) s = s.pipe(plumber());
    s = s.pipe(jade());
    s = s.pipe(rename({basename: 'index'}));
    s = s.pipe(gulp.dest(dest));
    if(develop) s = s.pipe(connect.reload());
    return s;
  }
}

function js(develop, src, dest){
  return function(){
    var webpackConfig = require('./webpack.config.js');
    webpackConfig.plugins = webpackConfig.plugins || [];
    webpackConfig.plugins.unshift(new webpackLib.DefinePlugin({DEBUG: develop}));

    var s = gulp.src(src);
    if(develop) s = s.pipe(plumber());
    s = s.pipe(webpack(webpackConfig));
    if(!develop) s = s.pipe(uglify());
    s = s.pipe(gulp.dest(dest));
    if(develop) s = s.pipe(connect.reload());
    return s;
  }
}

function css(develop, src, dest){
  var cssPipe = [prefixer()];
  if(!develop){
    cssPipe.push(mqpacker);
    cssPipe.push(csswring);
  }

  return function(){
    var s = gulp.src(src);
    if(develop) s = s.pipe(plumber());
    s = s.pipe(sass({errLogToConsole: true, includePaths: ["bower_components/foundation/scss"]}))
    s = s.pipe(postcss(cssPipe));
    s = s.pipe(gulp.dest(dest));
    if(develop) s = s.pipe(connect.reload());
    return s;
  }
}

gulp.task('dev-html', html(true, 'develop.jade', developDir));
gulp.task('dev-js', js(true, 'main.ts', developDir));
gulp.task('dev-css', css(true, 'styles/main.scss', developDir));

gulp.task('develop', ['dev-html', 'dev-js', 'dev-css'], function(){
  gulp.watch(['develop.jade', 'example.json'], ['dev-html']);
  gulp.watch(['main.*', 'components/*'], ['dev-js']);
  gulp.watch(['styles/*'], ['dev-css']);

  connect.server({
    root: 'develop',
    livereload: true
  });
});

gulp.task('css', css(false, 'styles/main.scss', tmpDir));
gulp.task('js', js(false, 'main.ts', tmpDir));
gulp.task('html', ['css', 'js'], html(false, 'production.jade', tmpDir));

gulp.task('template', ['html'], function(){
  return gulp.src(path.join(tmpDir, 'index.html'))
    .pipe(run('./template.hs', {silent: true}))
    .pipe(rename('template.html'))
    .pipe(gulp.dest(productionDir));
});

gulp.task('default', ['template']);

gulp.task('clean', function(cb){
  return del([developDir, tmpDir, productionDir], cb);
});
