"use strict";

$.cookie.json = true;
$.cookie.defaults.path = window.location.pathname;
$.cookie.defaults.expires = 365;

// save/restore panel state
(function(){
  function get_panel_ids_common(selector){
    var ids = [];
    $(selector).each(function(){
      ids.push($(this).attr('id'));
    });
    return ids;
  }

  function get_opened_panel_ids () {
    return get_panel_ids_common('.collapse.in');
  }

  function get_all_panel_ids(){
    return get_panel_ids_common('.collapse');
  }

  function open_panel_by_id(id){
    $('[id="' + id + '"]').addClass('in');
  }

  function save_panel_state(){
    $.cookie('panel', get_opened_panel_ids());
  }

  function restore_panel_state(){
    $.each($.cookie('panel') || [], function(){
      open_panel_by_id(this);
    });
  }

  function close_all_panels(){
    $('.collapse.in').collapse('hide');
    save_panel_state([]);
  }

  function open_all_panels(){
    $('.collapse').not('.in').collapse('show');
    save_panel_state(get_all_panel_ids());
  }

  function toggle_all_panels(){
    get_opened_panel_ids().length == 0 ? open_all_panels() : close_all_panels();
  }

  $(function(){
    restore_panel_state();
    $('.collapse').on('shown.bs.collapse', save_panel_state);
    $('.collapse').on('hidden.bs.collapse', save_panel_state);
    $('h1').click(toggle_all_panels);
  });

})();

// request test
if (location.protocol.slice(0,4) == "http") {(function(){

  $(function(){
    var update_test_route_functions = [];
    var update_test_query_functions = [];

    $('.panel').each(function(){
      // add elements
      var panel = $(this);
      var panel_id = panel.find('.panel-collapse').attr('id');
      panel.find('.route-parameters tr:first').append($('<th>value</th>'));
      panel.find('.route-parameters tr:gt(0)').append($('<td><input class="form-control input-sm" /></td>'));

      panel.find('.method').each(function(){
        var method = $(this);
        var h4     = method.find('h4:first');
        var method_str = h4.text();
        h4.remove();

        method.find('.action').each(function(){
          var action = $(this);
          action.detach();
          var action_wrapper = $('<div class="action-wrapper">');
          method.append(action_wrapper);

          var div = $('<div class="request-test clearfix">');
          div.append($('<button class="test-method btn btn-default">'+method_str+'</button>'));
          var span = $('<span class="route-string">');
          div.append(span);
          span.append($('<span class="route-url"></span>'));
          span.append($('<span class="route-params"></span>'));

          action_wrapper.append(div);
          action_wrapper.append(action);

          var callout = $('<div class="hidden request-result bs-callout"><div class="clearfix title"><h4 class="pull-left"></h4></div><div class="well headers"><pre><code></code></pre></div><div class="body"><pre><code></code></pre></div></div>');
          var close = $('<div class="close-btn pull-right">&times;</div>');
          callout.find('.title').append(close);
          close.click(function(){callout.addClass('hidden')});
          method.find('.action:last').append(callout);

          action.find('.query-parameters tr:first').append($('<th>value</th>'));
          action.find('.query-parameters tr:gt(0)').append($('<td><div class="input-group"><span class=\"input-group-addon\"><input type="checkbox"/></span><input class="form-control input-sm"/></div></td>'));
        });
      });

      // route parameters
      var route_params = {};
      panel.find('.route-parameters tr:gt(0)').each(function(){
        var p = $(this);
        route_params[p.find('td:first').text()] = p.find('td:last input');
      });

      function get_route(title){
        var ret = "";
        title.children().each(function(){
          var p = $(this);
          if (p.hasClass('splitter')) { ret += "/"; }
          else if (p.hasClass('path')) {ret += p.text(); }
          else if (p.hasClass('fetch')) {
            ret += route_params[p.text()].val();
          }
        });
        return ret;
      }

      var route_template = panel.find('.panel-title');
      var route_url      = panel.find('.route-url');
      function update_test_route(){
        var route = get_route(route_template);
        route_url.text(route);
      }

      update_test_route_functions.push(update_test_route);

      function save_route_parameters() {
        var save = {};
        $.each(route_params, function(k,v){
          save[k] = v.val();
        });
        var rp = $.cookie('route') || {};
        rp[panel_id] = save;
        $.cookie('route', rp);
      }

      panel.find('.route-parameters tr:gt(0) input')
        .keyup(update_test_route)
        .change(save_route_parameters);

      panel.find('.method').each(function(){

        // query parameters
        var method = $(this);
        var method_str = method.find('.test-method:first').text();

        var action_wrapper = method.find('.action-wrapper');
        var action_length = action_wrapper.length;
        action_wrapper.each(function(action_index){
          var action = $(this);
          var query_params = {};
          action.find('.query-parameters tr:gt(0)').each(function(){
            var p = $(this);
            var checkbox = p.find('td:last input[type="checkbox"]');
            var input    = p.find('td:last input.form-control');
            query_params[p.find('td:first').text()] = {checkbox: checkbox[0], input: input};
          });

          function get_query_param(){
            var ret = "";
            $.each(query_params, function(name, value){
              if(value.checkbox.checked){
                var val = value.input.val();
                if(val.length == 0) {
                  ret += "&" + name;
                } else {
                  $.each(val.split(' '), function(){
                    ret += "&" + name + "=" + this;
                  });
                }
              }
            });
            if(ret.length > 0) { ret = '?' + ret.slice(1) }
            return ret;
          }

          var query_param = action.find('.route-params');
          function update_query_param(){
            query_param.text(get_query_param());
          }
          update_test_query_functions.push(update_query_param);

          function save_query_param(){
            var save = {};
            $.each(query_params, function(name, value){
              save[name] = {checkbox: value.checkbox.checked, value: value.input.val()};
            });
            var qp = $.cookie('query') || {};
            if (!qp[panel_id]) { qp[panel_id] = {}; }
            if (!qp[panel_id][method_str]) { qp[panel_id][method_str] = new Array(action_length); }
            qp[panel_id][method_str][action_index] = save;
            $.cookie('query', qp)
          }

          function update_and_save_query_param(){
            update_query_param();
            save_query_param();
          }

          action.find('.query-parameters input[type="checkbox"]')
            .change(update_and_save_query_param);
          action.find('.query-parameters input.form-control')
            .keyup(update_query_param)
            .change(save_query_param);

          // request
          var action_btn = action.find('.test-method');
          var route_url  = action.find('.route-url');
          var result     = action.find('.request-result');
          var accept     = action.find('.precondition-accept span');
          function request_test () {
            if (action_btn.hasClass('disabled')){return}
            var url = route_url.text() + query_param.text();
            var meth  = method_str == "*" ? "GET" : method_str;
            action_btn.addClass('disabled');

            function request_callback (data, status, xhr) {
              var ct = xhr.getResponseHeader('Content-Type');
              var code = xhr.status;
              result.find('h4').text(code + ' ' + status + ' "' + ct + '"');

              result.find('.headers code').text(xhr.getAllResponseHeaders());

              if(ct.slice(0,16) == 'application/json') {
                try {
                  var json = JSON.stringify(JSON.parse(data), undefined, 2);
                  result.find('.body code').text(json);
                } catch (e) {
                  result.find('.body code').text(e + "\n\n" + data);
                }
              } else {
                result.find('.body code').text(data);
              }
            }

            function beforeSend (xhr) {
              if(accept.length != 0) {
                xhr.setRequestHeader('Accept', accept.text());
              }
            }

            $.ajax({type: meth, url: url, dataType: 'text', beforeSend: beforeSend})
              .done(function(data, status, xhr){
                request_callback(data, status, xhr);
                result.addClass('bs-callout-primary');
                result.removeClass('bs-callout-danger');
              })
              .fail(function(xhr, status, err){
                request_callback(err, status, xhr);
                result.addClass('bs-callout-danger');
                result.removeClass('bs-callout-primary');
              })
              .always(function(){
                result.removeClass('hidden');
                action_btn.removeClass('disabled');
              });
          }
          action.find('.test-method').click(request_test);
          action.find('input.form-control').keypress(function(e){
            if (e.which == 13) { request_test(); }
          });

        });
      });
    });

    function restore_route_parameters(){
      $.each($.cookie('route') || {}, function(id, dict){
        var panel = $('[id="' + id + '"]');
        panel.find('.route-parameters tr:gt(0)').each(function(){
          var row = $(this);
          var key = row.find('td:first').text();
          row.find('td:last input').val(dict[key] || "");
        });
      });
    }

    function restore_query_parameters(){
      try {
        $.each($.cookie('query') || {}, function(id, dict){
          var panel = $('[id="' + id + '"]');
          panel.find('.method').each(function(){
            var method = $(this);
            var method_str = method.find('.test-method:first').text();

            var action = method.find('.action-wrapper');
            if(action.length != dict[method_str].length) { throw "action length mismatch"; }
            action.each(function(idx){
              $(this).find('.query-parameters tr:gt(0)').each(function(){
                var row = $(this);
                var key = row.find('td:first').text();
                var checkbox = row.find('td:last input[type="checkbox"]')[0];
                var input    = row.find('td:last input.form-control');
                if (dict[method_str][idx]) {
                  checkbox.checked = dict[method_str][idx][key].checkbox || false;
                  input.val(dict[method_str][idx][key].value || "");
                }
              });
            });

          });
        });
      } catch (e) {
        console.log(e);
        $.removeCookie('query');
      }
    }

    restore_route_parameters();
    restore_query_parameters();
    $.each(update_test_route_functions, function(){this()});
    $.each(update_test_query_functions, function(){this()});
  });
})();}
