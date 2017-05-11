---
title: Lemming pants – specification
author:
- Andreas Ekeroot
abstract: |
    This document describes how the speaker list system for Datateknologsektionen should work.
...

# Registration

Members of Datateknologsektionen registers themselves in the system on their cell phones or on a terminal placed in the meeting hall.

## Sketch

<div class="example">
    <p>
        <label>CID
            <br><input type="text" />
        </label>
    </p>

    <p>
        <label>Speaker id
            <br><input type="text" />
        </label>
    </p>

    <p>
        <input type="submit" value="Register!">
    </p>
</div>

1. We use the `CID` to get the person's name from Chalmers LDAP.
1. We use the `Speaker id` to map the user in the system to an integer which is used by the deputee chairman to see who wants to talk.

This view is used to register meeting participants in the system, this is the most low tech, but automated solution so far.

