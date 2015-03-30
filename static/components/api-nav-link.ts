var Vue = require('Vue');

var ApiPath = require('./api-path.ts');
var animatedScrollTo = require('animated-scrollto');

var documentElement = document.body.scrollTop ? document.body : document.documentElement;

module.exports = Vue.extend({
  template: require('./api-nav-link.jade')(),
  methods: {
    jump: function(e){
      var target = document.getElementById(e.targetVM.anchor);
      animatedScrollTo(documentElement, target.offsetTop, 250);
    }
  },
  compiled: function(){
    if(this.$data.type === 'group') this.$el.className = 'heading';
  },
  watch: {
    active: function(active){
      if(this.$data.type === 'path'){
        this.$el.className = active ? 'active' : '';
      }
    }
  },
  events: {
    'active-route-changed': function(val){
      this.$set('active', this.anchor === val);
    }
  },
  computed: {
    anchor: function(){
      return ApiPath.anchorString(this.$data.path || []);
    }
  }
});
