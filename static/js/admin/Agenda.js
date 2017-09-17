import m from 'mithril';

import SpeakerQueueStack from './SpeakerQueueStack';

function requestHelper(url, method) {
    return m.request({
        method: method,
        url: url,
        headers: {
            'Accept': 'application/json',
            'Content-type': 'application/json'
        }
    });
}

function updateAgenda(a) {
    a['speakerQueueStack'] = SpeakerQueueStack(a['speakerQueueStack'], a);
    Agenda.currentItem = a;
}

// Data model
var Agenda = {
    currentItem: null,

    init: function() {
        requestHelper('/agenda/', 'GET').then(updateAgenda);
    },

    getCurrent: function() {
        if(this.currentItem !== null) {
            return this.currentItem;
        } else {
            throw Agenda.NotLoadedYetException;
        }
    },

    next: function() {
        requestHelper('/agenda/next/', 'POST').then(updateAgenda);
    },

    previous: function() {
        requestHelper('/agenda/previous/', 'POST').then(updateAgenda);
    },

    // Exceptions and other nice things.
    NotLoadedYetException: {
        message: 'The Agenda hasn\'t been loaded yet, probably due to space radiation.',
        name: 'NotLoadedYetException'
    }
}

export default Agenda;
