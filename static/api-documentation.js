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
