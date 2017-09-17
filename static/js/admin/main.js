'use strict';

import m from 'mithril';
import Agenda from './Agenda';
import AttendeeList from './AttendeeList';
import SpeakerQueueStack from './SpeakerQueueStack';

var AgendaComponent = {
    oninit: Agenda.init,
    view: function() {
        try {
            return m('section', [
                m('h2', [
                    m('button', { onclick: function() { Agenda.previous(); } }, '|<'),
                    m.trust('&sect;'),
                    (Agenda.getCurrent().order + 1) + '. ',
                    Agenda.getCurrent().title,
                    m('button', { onclick: function() { Agenda.next(); } }, '>|')
                ])
            ]);
        } catch(e) {
            if(e.name === 'NotLoadedYetException') {
                return '';
            } else {
                throw e;
            }
        }
    }
}

var AttendeeListComponent = {
    visible: false,
    oninit: AttendeeList.init,
    view: function() {
        return m('section', [
            m('p', [
                m('a[href=""]', { onclick: function() {  } }, 'Show list of attendees.')
            ])
        ]);
    }
}

var SpeakerQueueStackComponent = {
    view: function() {
        try {
            const currSpeaker = Agenda.getCurrent()['speakerQueueStack'].getCurrent();
            return m('section', [
                m('h1', 'Speakers'),
                m('p', ['Current speaker: ', currSpeaker ? currSpeaker.cid : '---']),
                 m('button', { onclick: function() { Agenda.getCurrent()['speakerQueueStack'].dequeueSpeaker() } }, 'POP!'),
                m('ol', Agenda.getCurrent()['speakerQueueStack'].list().map(function(a) {
                    return m('li', [
                        m('button', { onclick: function() { Agenda.getCurrent()['speakerQueueStack'].removeSpeaker(a.id) } }, 'DEL'),
                        ' ',
                        a.cid
                    ]);
                })),
                m('p', ['Speaker stack height: ', Agenda.getCurrent()['speakerQueueStack'].stackHeight()]),
                m('button', { onclick: function() { Agenda.getCurrent()['speakerQueueStack'].push(); } }, 'Push speaker queue'),
                m('button', { onclick: function() { Agenda.getCurrent()['speakerQueueStack'].pop(); } }, 'Pop speaker queue'),
                m('p', [
                    m('label', [
                        'Attendee number',
                        m('br'),
                        m('input', { type: 'number', id: 'attendeeId' })
                    ])
                ]),
                m('p', [
                    m('input', {
                        onclick: function() {
                            Agenda
                                .getCurrent()['speakerQueueStack']
                                .enqueueSpeaker(document.querySelector('#attendeeId').value);
                        },
                        type: 'submit',
                        value: 'Add to speaker list!'
                    })
                ])
            ]);
        } catch(e) {
            if(e.name === 'NotLoadedYetException') {
                return '';
            } else {
                throw e;
            }
        }
    }
}

var MeetingAdmin = {
    view: function() {
        return [
            m(AgendaComponent),
            m(AttendeeListComponent),
            m(SpeakerQueueStackComponent)
        ];
    }
};
m.mount(document.body, MeetingAdmin);
