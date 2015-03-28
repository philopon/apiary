'use strict';

var gulp    = require('gulp');
var webpack = require('gulp-webpack');
var rename  = require('gulp-rename');
var jade    = require('gulp-jade');
var connect = require('gulp-connect');
var sass    = require('gulp-sass');
var plumber = require('gulp-plumber');

var postcss = require('gulp-postcss');
var autoprefixer = require('autoprefixer-core');

var developDir = "develop";

gulp.task('dev-html', function(){
  return gulp
    .src('develop.jade')
    .pipe(plumber())
    .pipe(jade())
    .pipe(rename({basename: "index"}))
    .pipe(gulp.dest(developDir))
    .pipe(connect.reload());
});

gulp.task('dev-js', function(){
  return gulp
    .src('main.ts')
    .pipe(plumber())
    .pipe(webpack(require('./webpack.config.js')))
    .pipe(gulp.dest(developDir))
    .pipe(connect.reload());
});

gulp.task('dev-css', function(){
  return gulp
    .src('styles/main.scss')
    .pipe(plumber())
    .pipe(sass({errLogToConsole: true, includePaths: ["bower_components/foundation/scss"]}))
    .pipe(postcss([autoprefixer()]))
    .pipe(gulp.dest(developDir))
    .pipe(connect.reload());
});

gulp.task('develop', ['dev-html', 'dev-js', 'dev-css'], function(){
  gulp.watch(['develop.jade', 'example.json'], ['dev-html']);
  gulp.watch(['main.*', 'components/*'], ['dev-js']);
  gulp.watch(['styles/*'], ['dev-css']);

  connect.server({
    root: 'develop',
    livereload: true
  });
});
