declare var module : any;

var Vue = require('Vue');

var definition = require('./define.js');
if(definition.debug){
  Vue.config.debug = true;
  console.warn('Debug mode');
}

Vue.filter('length', function(i){return i.length});

function getWindowHeight() : number {
  var w = window,
  d = document,
  e = d.documentElement,
  g = d.getElementsByTagName('body')[0];
  return w.innerHeight || e.clientHeight || g.clientHeight;
}

document.addEventListener('DOMContentLoaded', function(){

  var data = JSON.parse(document.getElementById('data').innerHTML);

  var vm = new Vue({
    template: require('./main.jade')(),
    data: {
      title: data.title,
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

  function scrollSpy(){
    var scrollTop = document.documentElement.scrollTop || document.body.scrollTop,
        windowHeight = getWindowHeight();

    for(var i = 0, l = offsetTops.length; i < l; i++){
      var o = offsetTops[i];
      if(scrollTop > o.offsetTop - windowHeight / 3) {
        vm.$set('navbar', o.id);
        break;
      }
    }
  }

  vm.$mount(document.createElement('x-document'));

  var offsetTops = [];
  vm.$appendTo('body', function(){
    for(var i = 0, l = vm.routes.length; i < l; i++) {
      var r = vm.routes[i];
      offsetTops.unshift({offsetTop: r.offsetTop, id: r.id});
    }
    scrollSpy();
  });

  document.addEventListener('scroll', scrollSpy);

});
