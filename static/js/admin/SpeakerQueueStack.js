'use strict';

import m from 'mithril';

// A function creating an object that can handle a speaker queue stack.
function SpeakerQueueStack(s, a) {
    function postHelper(me, url) {
        m.request({
            method: 'POST',
            url: url,
            headers: {
                'Accept': 'application/json',
                'Content-type': 'application/json'
            }
        }).then(function(s) {
            me.stack = s;
        });
    }

    return {
        stack: s,
        agenda: a,

        // Push a new speaker queue onto the stack.
        push: function() {
            postHelper(this, '/agenda/' + this.agenda.id + '/speakerQueue/push/');
        },

        // Pop the top speaker queue of the stack.
        pop: function() {
            postHelper(this, '/agenda/' + this.agenda.id + '/speakerQueue/pop/');
        },

        // Enqueue a speaker to the end of the top speaker queue.
        enqueueSpeaker: function(attendeeId) {
            postHelper(this, '/agenda/' + this.agenda.id + '/speaker/enqueue/' + attendeeId);
        },

        // Deque a speaker from the front of the top speaker queue.
        dequeueSpeaker: function() {
            postHelper(this, '/agenda/' + this.agenda.id + '/speaker/dequeue/');
        },

        // Remove a speaker from the top speaker queue.
        removeSpeaker: function(attendeeId) {
            postHelper(this, '/agenda/' + this.agenda.id + '/speaker/remove/' + attendeeId);
        },

        // List all speakers of the topmost queue.
        list: function() {
            return this.stack[0]['speakers'];
        },

        // Return the current speaker.
        getCurrent: function() {
            return this.stack[0]['current'];
        },

        stackHeight: function() {
            return this.stack.length;
        }
    }
};

export default SpeakerQueueStack;
