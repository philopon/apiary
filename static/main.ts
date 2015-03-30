declare var module : any;

var Vue = require('Vue');
var debounce = require('lodash/function/debounce');
var throttle = require('lodash/function/throttle');

var definition = require('./define.js');
if(definition.debug){
  Vue.config.debug = true;
  console.warn('Debug mode');
}

Vue.filter('length', function(i){return i.length});

var hasTouchEvent = 'ontouchstart' in window;

Vue.directive('click', {
  bind: function(){
    var _this = this;
    this.eventName = hasTouchEvent ? 'touchstart' : 'mousedown';

    this.handler = function () {
      this.targetVM = _this.vm;
      _this.vm.$eval(_this.expression)(this);
    }

    this.el.addEventListener(this.eventName, this.handler);
  },
  unbind: function(){
    this.el.removeEventListener(this.eventName, this.handler);
  }
});

function getWindowSize() : any {
  var w = window,
  d = document,
  e = d.documentElement,
  g = d.getElementsByTagName('body')[0];
  return {
    height: w.innerHeight || e.clientHeight || g.clientHeight,
    width:  w.innerWidth  || e.clientWidth  || g.clientWidth
  };
}

document.addEventListener('DOMContentLoaded', function(){

  var data = JSON.parse(document.getElementById('data').innerHTML);

  var vm = new Vue({
    template: require('./main.jade')(),
    data: {
      title: data.title,
      description: data.description,
      touch: hasTouchEvent,
      data: data.data,
      anchorSplitter: ':',
      routes: []
    },
    watch: {
      navbar: function(val, old){
        this.$broadcast('active-route-changed', val);
      }
    },
    components: {
      'api-route': require('./components/api-route.ts'),
      'api-nav-link': require('./components/api-nav-link.ts')
    },
    computed: {
      sideNav: function(){
        var out = [];
        for(var i = 0, l = this.$data.data.length; i < l; i++) {
          var datum = this.$data.data[i];
          if(datum.group){
            out.push({type: 'group', name: datum.group})
          }
          for(var j = 0, m = datum.paths.length; j < m; j++){
            var path = datum.paths[j];
            out.push({type: 'path', path: path.path});
          }
        }
        return out;
      }
    }
  });

  var title = vm.$addChild({
    el: 'title',
    template: '{{*title}}',
    inherit: true
  });

  vm.$mount(document.createElement('x-document'));

  var offsetTops = [];
  var windowSize = getWindowSize();
  windowSize.registered = false;

  vm.$appendTo('body', function(){
    for(var i = 0, l = vm.routes.length; i < l; i++) {
      var r = vm.routes[i];
      offsetTops.unshift({offsetTop: r.offsetTop, id: r.id});
    }
    scrollSpy();
  });

  window.addEventListener('resize', debounce(function(){
    var newSize = getWindowSize();
    windowSize.width = newSize.width;
    windowSize.height = newSize.height;
    registerSpy();
  }, 200));

  function scrollSpy(){
    var scrollTop = document.documentElement.scrollTop || document.body.scrollTop;

    for(var i = 0, l = offsetTops.length; i < l; i++){
      var o = offsetTops[i];
      if(scrollTop > o.offsetTop - windowSize.height / 3) {
        vm.$set('navbar', o.id);
        break;
      }
    }
  }

  var throttleSpy = throttle(scrollSpy, 500);

  function registerSpy(){
    if(windowSize.width >= 600) {
      if (!windowSize.registered) {
        windowSize.registered = true;
        document.addEventListener('scroll', throttleSpy);
      }
    } else {
      if (windowSize.registered) {
        windowSize.registered = false;
        document.removeEventListener('scroll', throttleSpy);
      }
    }
  }

  registerSpy();

});
