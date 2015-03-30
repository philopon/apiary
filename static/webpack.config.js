var webpack = require('webpack');

module.exports = {
  output: {
    filename: '[name].js'
  },
  module: {
    loaders: [
      {test: /\.jade$/, loader: 'jade-loader'},
      {test: /\.ts$/, loader: 'typescript-loader'}
    ]
  },
  plugins: [
    new webpack.DefinePlugin({DEBUG: true}),
    new webpack.optimize.OccurenceOrderPlugin()
  ]
}
