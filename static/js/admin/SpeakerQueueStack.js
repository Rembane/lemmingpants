'use strict';

import m from 'mithril';

// A function creating an object that can handle a speaker queue stack.
function SpeakerQueueStack(s, a) {
    return {
        stack: s,
        agenda: a,

        // Push a new speaker queue onto the stack.
        push: function() {
            var me = this;
            m.request({
                method: 'POST',
                url: '/agendaitem/pushSpeakerQueue',
                data: me.agenda.id,
                headers: {
                    'Accept': 'application/json',
                    'Content-type': 'application/json'
                }
            }).then(function(s) {
                me.stack = s;
            });
        },

        // Pop the top speaker queue of the stack.
        pop: function() {
            this.stack.shift();
        },

        // Push a speaker to the end of the top speaker queue.
        pushSpeaker: function(attendee) {
            this.stack[0]['speakers'].push(attendee);
        },

        // Remove and return a speaker from the front of the top speaker queue.
        speak: function() {
            return this.stack[0]['speakers'].shift();
        },

        // List all speakers of the topmost queue.
        list: function() {
            return this.stack[0]['speakers'];
        },

        stackHeight: function() {
            return this.stack.length;
        }
    }
};

export default SpeakerQueueStack;
