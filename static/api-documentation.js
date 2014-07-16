(function(){
  var unique = function(input){
    var u = {}, a = [];
    for(var i = 0, l = input.length; i < l; ++i){
      if(u.hasOwnProperty(input[i])) continue;
      a.push(input[i]);
      u[input[i]] = 1;
    }
    return a;
  }

  var remove = function(input, v){
    var a = [];
    for(var i = 0; i < input.length; i++){
      if(input[i] != v) a.push(input[i]);
    }
    return a;
  }

  $.cookie.json = true;
  $.cookie.defaults.expires = 365;

  $(function(){
    var current = $.cookie('open') || [];

    $('.collapse').on('show.bs.collapse', function(e){
      var cur = e.target.id.slice(9);
      current.push(cur);
      current = unique(current);
      current.sort();
      $.cookie('open', current);
    }).on('hide.bs.collapse', function(e){
      var cur = e.target.id.slice(9);
      current = remove(current, cur);
      $.cookie('open', current);
    });

    $('h1').click(function(){
      if(current.length == 0) {
        $('.collapse').each(function(i,e){
          $(e).collapse('show');
          var cur = e.id.slice(9);
          current.push(cur);
        }).promise().done(function(){
          current = unique(current);
          current.sort();
          $.cookie('open', current);
        });
      } else {
        $('.collapse').each(function(i,e){
          $(e).collapse('hide');
        });
        current = [];
        $.cookie('open', []);
      }
    });

    for(var i = 0; i < current.length; i++) {
      $("[id='collapse-" + current[i] + "']").addClass('in');
    }
  });

})();
