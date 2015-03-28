var Vue = require('Vue');

var ApiPath = require('./api-path.ts');

module.exports = Vue.extend({
  template: require('./api-nav-link.jade')(),
  components: {
    'api-path': ApiPath
  },
  methods: {
    jump: function(e){
      var target = document.getElementById(e.targetVM.anchor);
      window.scrollTo(0, target.offsetTop);
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
