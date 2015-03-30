var Vue  = require('Vue');

function searchParent(elm: Node, target: Node) : boolean{
  var t = target;
  while(t){
    if(t === elm){
      return true;
    }
    t = t.parentNode;
  }
  return false;
}

module.exports = Vue.extend({
  paramAttributes: ['placeholder'],
  data: function(){
    return { visibleOptions: false }
  },
  template: require('./combobox.jade')(),
  methods: {
    toggleOptions: function(){
      this.$data.visibleOptions = !this.$data.visibleOptions;
    },
    selectOption: function(e){
      this.$data.model = e.targetVM.$data.o;
      this.$data.visibleOptions = false;
    },
  },
  computed: {
    isComboBox: function(){
      var options = this.$data.options;
      if(options && options.length > 0) return true;
      return false;
    }
  },
  compiled: function(){
    var _this = this;
    var elm = this.$el;
    document.addEventListener('click', function(e){
      if(!searchParent(elm, <Node>e.target)){
        _this.$set('visibleOptions', false);
      };
    });
  }
});
