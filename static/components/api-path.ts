var Vue = require('Vue');

type piece = {tag: string; path: string; name: string};

function pieceToString(data : piece) : string {
  var tag  = data.tag;
  if(tag === "path") {
    return data.path;
  } else if (tag === "fetch") {
    return data.name;
  } else {
    return "**" + data.name;
  }
}

function anchorString(path : [piece]) : string{
  var out = "";

  for(var i = 0, l = path.length; i < l; i++) {
    var p = path[i];
    out += '/' + pieceToString(p);
  }

  return out;
}

var piece = Vue.extend({
  template: require('./api-path-piece.jade')(),
  computed: {
    pathPiece: function(){
      return pieceToString(this.$data);
    },
    labelFor: function(){
      if(!this.$parent.attachLabel) { return };

      var anchor = this.$parent.anchor,
          data   = this.$data,
          tag    = data.tag;

      if(tag === 'fetch' || (tag === 'rest' && data.name)){
        return anchor + this.$root.$data.anchorSplitter + data.name;
      }
    }
  }
});

module.exports = Vue.extend({
  template: '<api-path-piece data-v-repeat="path"></api-path-piece>',
  components: {'api-path-piece': piece},
  computed: {
    anchor: function(){
      return anchorString(this.$data.path || []);
    }
  }
});

module.exports.anchorString = anchorString;
