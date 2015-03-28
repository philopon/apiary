var Vue = require('Vue');

Vue.config.prefix = 'data-v-';

var ApiRouteParam = Vue.extend({
  data: function(){
    return {value: ""}
  },
  template: require('./api-route-param.jade')(),
  methods: {
    checkInput: function(e){
      var tag = this.$data.tag, key = e.which || e.keyCode;
      if(tag !== 'rest' && key == 191) e.preventDefault();
    }
  },
  computed: {
    anchor: function(){
      return this.$parent.anchor + this.$root.anchorSplitter + this.$data.name;
    }
  }
});

var RouteParameterFilter = function(inp){
  var out = [];
  for(var i = 0, l = inp.length; i < l; i++){
    var data = inp[i], tag = data.tag;
    if(tag === 'fetch' || (tag === 'rest' && data.name)) out.push(data);
  }
  return out;
}

var ApiPath = require('./api-path.ts');

module.exports = Vue.extend({
  template: require('./api-route.jade')(),
  components: {
    'api-path': ApiPath,
    'api-route-param': ApiRouteParam,
    'api-method': require('./api-method.ts')
  },
  filters: {
    'route-parameter': RouteParameterFilter
  },
  computed: {
    route: function(){
      var paramComponents = this.$.param || [],
          params          = {};
      for(var i = 0, l = paramComponents.length; i < l; i++){
        var pc = paramComponents[i];
        params[pc.name] = pc.value;
      }

      var route = "";
      var pathPiecies = this.$data.path;
      for(var i = 0, l = pathPiecies.length; i < l; i++){
        var pp = pathPiecies[i];
        route += '/';
        if(pp.tag === 'path'){
          route += pp.path;
        } else if(pp.tag !== 'rest') {
          var val = '', raw = params[pp.name];
          for(var j = 0, m = raw.length; j < m; j++) {
            if(raw[j] !== '/') val += raw[j];
          }
          route += val;
        } else {
          route += params[pp.name];
        }
      }

      return route;
    },
    anchor: function(){
      return ApiPath.anchorString(this.$data.path)
    }
  },
  compiled: function(){
    var el = this.$el;
    el.id = this.anchor;
    this.$root.routes.push(el);
  }
});
