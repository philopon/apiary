var Vue = require('Vue');
var SuperAgent = require('superagent');

var ApiQueryParam = Vue.extend({
  data: function(){
    return {enabled: false, value: "", inputted: false}
  },
  template: require('./api-query-param.jade')(),
  methods: {
    toggleEnabled: function(e){
      if(e.target.nodeName !== "INPUT")
        this.$data.enabled = !this.$data.enabled;
    },
    inputEnabling: function(){
      if(!this.$data.inputted){
        this.$data.enabled = true;
      }
      this.$data.inputted = true;
    }
  },
  computed: {
    queryType: function(){
      var data  = this.$data,
          query = data.query,
          typ   = data.type;
      if(query === "nullable") {
        typ += "?";
      } else if (query === "check") {
        typ = "exists";
      } else if (query === "noValue") {
        typ = "-";
      }
      return typ;
    },
    anchor: function(){
      return this.$parent.anchor + this.$root.anchorSplitter + this.$data.name;
    }
  },
  created: function(){
    var storage = window.localStorage;
    var enabledKey = "enabled" + this.$root.anchorSplitter + this.anchor;

    this.value   = storage.getItem(this.anchor) || "";
    this.enabled = !!storage.getItem(enabledKey) || false;

    this.$watch('[enabled, value]', function(val){
      var enabled = val[0], value = val[1];
      if(enabled) {
        storage.setItem(enabledKey, "true");
      } else {
        storage.removeItem(enabledKey);
      }
      storage.setItem(this.anchor, value);

      this.$dispatch('query-param-updated', this.$parent.$index, this.$data.name, enabled, value);
    }, false, true);
  }
});

var ApiHandler = Vue.extend({
  template: require('./api-handler.jade')(),
  computed: {
    preconds: function(){
      var data     = this.$data,
          preconds = data.preconditions,
          accept   = data.accept;

      if(accept){
        preconds = ["accept: " + accept].concat(preconds);
      }
      return preconds;
    },
    anchor: function(){
      return this.$parent.anchor + this.$root.anchorSplitter + this.$index;
    }
  }
});

function queryParamsString(obj){
  var out = [];
  for(var k in obj) {
    var vs = obj[k];
    for(var i = 0, l = vs.length; i < l; i++){
      var v = vs[i];
      // out.push([k, v]);
      out.push(k + (v ? '=' + v : ''));
    }
  }
  if(out.length > 0){
    return '?' + out.join('&');
  }
}

module.exports = Vue.extend({
  data: function(){
    return {
      rawQueries: [],
      result: {},
      locked: false
      }
  },
  template: require('./api-method.jade')(),
  filters: {
    queryParamsString: queryParamsString
  },
  components: {
    'api-handler': ApiHandler,
    'api-query-param': ApiQueryParam,
    'x-combobox': require('./combobox.ts')
  },
  compiled: function(){
    this.$el.className = this.$data.method || 'any';
  },
  created: function(){
     var accepts = [], hs = this.$data.handlers;
    for(var i = 0, l = hs.length; i < l; i++){
      var a = hs[i].accept;
      if(a) accepts.push(a);
    }

    this.$set('accepts', accepts || []);
    this.$set('accept', accepts[0]);
  },
  events: {
    'route-updated': function(route){
      this.$set('route', route);
    },
    'query-param-updated': function(index, name, enabled, value){
      var key = 'rawQueries[' + index + ']["' + name + '"]';
      this.$set(key, {enabled: enabled, value: value});
    },
    'request:start': function(){
      this.$set('locked', true);
    },
    'request:done': function(){
      this.$set('locked', false);
    }
  },
  computed: {
    anchor: function(){
      return this.$parent.anchor + this.$root.anchorSplitter + this.$data.method;
    },
    queries: function(){
      var data = this.$data, qs = this.$data.rawQueries, out = {};
      for(var i = 0, l = qs.length; i < l; i++) {
        var q = qs[i];
        if (!q) continue;

        for(var k in q) {
          var v = q[k];
          if(!v.enabled) continue;

          out[k] = out[k] || [];
          var vs = v.value.split(',');
          for(var j = 0, m = vs.length; j < m; j++){
            var v = vs[j].trim();
            out[k].push(v.length === 0 ? null : v);
          }
        }
      }
      return out;
    },
    requested: function(){
      return !!this.$data.result.status;
    },
    disabled: function() {
      var data = this.$data;
      return !data.route || data.locked || location.protocol === 'file:';
    }
  },
  methods: {
    request: function(){
      var _this = this;
      var data = this.$data, method = data.method || 'GET';
      if(this.disabled) return;

      this.$parent.$broadcast('request:start');

      try {
        var req = SuperAgent(method, data.route);
        if(data.accept) req.accept(data.accept);

        var qs = this.queries;
        for(var k in qs) {
          for(var i = 0, l = qs[k].length; i < l; i++) {
            var obj = {};
            obj[k] = qs[k][i];
            if(method === 'POST' || method === 'PUT') {
              req.send(obj);
            } else {
              req.query(obj);
            }
          }
        }

        req.end(function(err, res){
          _this.$set('result', err || res);
        });
      } catch(e) {
        _this.$set('result', {
          status: 'XXX',
          message: e.name,
          response: {
            headers: {},
            body: e.message + '\n\n' + e.stack
          }
        });
      } finally {
       _this.$parent.$broadcast('request:done');
      }
    },
    dismiss: function(){ this.result = {}; }
  }
});
