declare var module : any;

var Vue = require('Vue');

Vue.config.debug = true;

Vue.filter('length', function(i){return i.length});

function getWindowHeight() : number {
  var w = window,
  d = document,
  e = d.documentElement,
  g = d.getElementsByTagName('body')[0];
  return w.innerHeight || e.clientHeight || g.clientHeight;
}


document.addEventListener('DOMContentLoaded', function(){

  var title = document.getElementsByTagName('title')[0].innerHTML;
  var data = JSON.parse(document.getElementById('data').innerHTML);

  var vm = new Vue({
    template: require('./main.jade')(),
    data: {
      title: title,
      data: data,
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
          for(var j = 0, m = datum.pathes.length; j < m; j++){
            var path = datum.pathes[j];
            out.push({type: 'path', path: path.path});
          }
        }
        return out;
      }
    }
  });

  var elm = document.createElement('x-document');
  vm.$mount(elm);
  document.body.appendChild(vm.$el);

  var offsetTops = [];
  for(var i = 0, l = vm.routes.length; i < l; i++) {
    var r = vm.routes[i];
    offsetTops.unshift({offsetTop: r.offsetTop, id: r.id});
  }

  document.addEventListener('scroll', function(){
    var scrollTop = document.documentElement.scrollTop || document.body.scrollTop;
    var windowHeight = getWindowHeight();

    for(var i = 0, l = offsetTops.length; i < l; i++){
      var o = offsetTops[i];
      if(scrollTop > o.offsetTop - windowHeight / 3) {
        vm.$set('navbar', o.id);
        break;
      }
    }
  });

});
