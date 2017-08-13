import m from 'mithril';

// Data model
var Agenda = {
    agenda: null,
    current: 0,

    init: function() {
        m.request({
            method: 'GET',
            url: '/agendaitem/list',
            headers: {
                'Accept': 'application/json',
                'Content-type': 'application/json'
            }
        }).then(function(as) {
            Agenda.agenda = as;
        });
    },

    getCurrent: function() {
        return this.agenda[this.current];
    },

    incCurrent: function() {
        this.current++;
    },

    decCurrent: function() {
        this.current--;
    }
}


export default Agenda;
