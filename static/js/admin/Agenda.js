import m from 'mithril';

import SpeakerQueueStack from './SpeakerQueueStack';

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
            Agenda.agenda = as.map(function(a) {
                a['speakerQueueStack'] = SpeakerQueueStack(a['speakerQueueStack'], a);
                return a;
            });
        });
    },

    getCurrent: function() {
        if(this.agenda !== null && this.agenda.length > 0) {
            return this.agenda[this.current];
        } else {
            throw Agenda.NotLoadedYetException;
        }
    },

    incCurrent: function() {
        this.current++;
    },

    decCurrent: function() {
        this.current--;
    },

    // Exceptions and other nice things.
    NotLoadedYetException: {
        message: 'The Agenda hasn\'t been loaded yet, probably due to space radiation.',
        name: 'NotLoadedYetException'
    }
}

export default Agenda;
