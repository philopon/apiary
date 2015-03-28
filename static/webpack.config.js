module.exports = {
  output: {
    filename: '[name].js'
  },
  module: {
    loaders: [
      {test: /\.jade$/, loader: 'jade-loader'},
      {test: /\.ts$/, loader: 'typescript-loader'}
    ]
  }
}
