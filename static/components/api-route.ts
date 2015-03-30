var Vue = require('Vue');

function watchValue(val){
  if(this.$data.tag === 'rest') {
    this.$dispatch('route-param-updated', this.name, val);
    this.$set('error', false);
    return;
  }

  var len = 0;
  for(var i = 0, l = val.length; i < l; i++){
    if(val[i] === '/') { len = 0; break; }
    len += 1;
  }

  var v = len === 0 ? null : val;
  this.$dispatch('route-param-updated', this.name, v);
  this.$set('error', len === 0);
}

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
  created: function(){
    this.$watch('value', watchValue, false, true);
    this.$data.value = window.localStorage.getItem(this.anchor) || "";
  },
  watch: {
    value: function(value){
      window.localStorage.setItem(this.anchor, value);
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

function applyRouteParams(path : [{tag: string; path: string; name: string}], params: {key?: string}) : string {
  var out = '';
  for(var i = 0, l = path.length; i < l; i++){
    var piece = path[i], tag = piece.tag;
    out += '/';
    if(tag === 'path') {
      out += piece.path;
    } else if (tag === 'fetch') {
      var v = params[piece.name];
      if (v && v.length > 0) { out += v } else { return null };
    } else if (tag === 'rest' && piece.name) {
      out += params[piece.name] || '';
    }
  }
  return out;
}

module.exports = Vue.extend({
  data: function(){
    return {routeParams: {}}
  },
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
    anchor: function(){
      return ApiPath.anchorString(this.$data.path)
    }
  },
  events: {
    'route-param-updated': function(name, value){
      var data   = this.$data,
          params = data.routeParams;
      params[name] = value;

      var route = applyRouteParams(data.path, params);
      this.$broadcast('route-updated', route);
    }
  },
  compiled: function(){
    // data for scroll spy
    var el = this.$el;
    el.id = this.anchor;
    this.$root.routes.push(el);

    // initial send route-updated
    var data = this.$data;
    var route = applyRouteParams(data.path, data.routeParams);
    this.$broadcast('route-updated', route);
  }
});
