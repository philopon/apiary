var Vue = require('Vue');

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
  }
});

module.exports = Vue.extend({
  template: require('./api-method.jade')(),
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
    this.$set('accept', accepts[0] || 'text/html');
  },
  computed: {
    anchor: function(){
      return this.$parent.anchor + this.$root.anchorSplitter + this.$data.method;
    }
  }
});
