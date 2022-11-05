# Server

fuck off


## Technical design

### db




### misc

Upload a picture. Then, host it as a url. Not very private, but this should make it easier (not mucking about with multipart/form-data too much) and make responses lightweight.

Okay, I think, I've decided. That's our app anyway:

1.  Only one match per request. The last match is saved. You *have* to yay or nay this match to get the next one. (currentMatch (or just match)/(respond/decide/whatever)) Simple and fun. 



## Design Notes

### How it should work

I'll try to find out how this thing will be used. So:

1. send next bachelor 
 - one or more? technically, a list of bachelors might be used in the real world context, because it batches the candidates. 
 - Negative: remaining bachelors might be inaccurate
 - returns: name, height, image, description (short version?), age -- just fuck it up, the more vapid it is, the better

2. reply with Yea or Nay 
 - should it have some limitations, like you can only reply to an "active" (ie. previously returned) person? 
   - an "active" flag is not a bad idea with multiple bachelors. 
   - bad stuff - multiple updates. or maybe a third state, like "Yea", "Nay" and "Considering"? less space, no invalid state - answered, but active.
   - do we allow previously declined people to appear in the algorithm?

3. Register / Log in
  - some form of tokens; refresh (active date extended) on every activity
  - registering a profile means filling out some basic shit, whatever
  - have to upload an image - uh oh

4. Chat
  - I'll have to look up an example of how do it with Websockets. Gotta learn it someday!
  - start a chat *only* with someone you've seen. Can't search people, faggot.
  - block people (important for the *feel* of the app)


---------------

### ok, insell. What now?

Huh, I wonder what user-user interactions does tinder track. Like, superlikes or some shit?

https://www.help.tinder.com/hc/en-us/categories/115000755686-A-Guide-To-Tinder-

Wtf, surprisingly helpful.

Interesting, can't find specific people. So, only if both people do the Yay! thing, only then can you message someone. Maybe add it to the POST method? How tf does it work. If someone "likes" you, then the second person gets a notification? Who starts the conversation? Async problems (liking someone at the same time, needs TVars or some database specific sync stuff)?

Possible to add other photos? How can they be viewed? Maybe not - we don't need 'em.

There's something like "**Interests**". I can use it in the algorithm to generate matches. Possible to edit. Predefined? Or maybe possible to create new, and other users can search for it.

There's also somehting like "lifestyles". Same thing, basically.

God fucking dammit, """people""" who use tinder are literal subhuman trash. 

![jegor letow](https://en.wikipedia.org/wiki/Yegor_Letov#/media/File:Egor_Letov_2000-11-04_Nuernberg.jpg)

> И вся грязь превратилась в голый лёд
> И всё идёт по плану 
> Всё идёт по плану


Algorithm values (according to the website):
 - activity (makes sense, but I don't care, really)
 - photos (image recognition, not viable for our case)
 - "likes and nopes" - cool, we can assign an "undersirability" score to make it more artistic; fuck

What they don't track:
 - ethnicity (we can match on that, kek)

Messaging only possible when two yay eachother.

Unmatching someone - deletes all conversations with him. 


Premium features (funny):
 - superlike + ability to message someone before they like yo' ass
 - priority likes (wtf) - seen faster by potential match (so liking someone does not immediately "invoke" a webhook or whatever? I guess makes sense. "refresh" conversations and check if there's a "new match")
 - telss you you're liked by this match -
