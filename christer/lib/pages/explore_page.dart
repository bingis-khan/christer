import 'package:christer/persist/persist.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:christer/data/explore_json.dart';
import 'package:christer/data/icons.dart';
import 'package:christer/theme/colors.dart';
import 'package:flutter/scheduler.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:swipe_cards/draggable_card.dart';
import 'package:swipe_cards/swipe_cards.dart';
import 'package:christer/model/match.dart';

class ExplorePage extends StatefulWidget {
  @override
  _ExplorePageState createState() => _ExplorePageState();
}

class _ExplorePageState extends State<ExplorePage> {
  late Future<CardContext> _cardContext;

  @override
  void initState() {
    super.initState();
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    _cardContext = fillCards(UserContext.of(context));
  }

  @override
  Widget build(BuildContext context) {
    print('${UserContext.of(context).email}');
    return FutureBuilder(
      future: _cardContext,
      builder: ((context, snapshot) {
        Widget body;
        MatchEngine? engine;
        if (snapshot.hasData) {
          var cardContext = snapshot.requireData;
          var matches = cardContext.matches;

          if (cardContext.engine.currentItem == null) {
            body = niceLookingSpinnerFuckYou();
          } else {
            body = getBody(cardContext.engine, matches);
            engine = cardContext.engine;
          }
        } else if (snapshot.hasError) {
          body = Text(snapshot.error.toString());
        } else {
          body = niceLookingSpinnerFuckYou();
        }

        return Scaffold(
          backgroundColor: black,
          body: mainCard(body),
          bottomSheet: getFooter(engine),
        );
      }),
    );
  }

  Widget niceLookingSpinnerFuckYou() => Container(
      alignment: Alignment.center,
      decoration:
          BoxDecoration(borderRadius: BorderRadius.circular(10), boxShadow: [
        BoxShadow(
          color: grey.withOpacity(0.3),
          blurRadius: 5,
          spreadRadius: 2,
        )
      ]),
      child: const CircularProgressIndicator());

  Widget mainCard(Widget inner) {
    var size = MediaQuery.of(context).size;
    return Padding(
        padding: const EdgeInsets.only(bottom: 1),
        child: Container(height: size.height * 0.75, child: inner));
  }

  Widget getBody(MatchEngine engine, List<Match> items) {
    var size = MediaQuery.of(context).size;
    var user = UserContext.of(context);
    return SwipeCards(
      matchEngine: engine,
      itemBuilder: (BuildContext context, int index) {
        Match current = items[index];

        return Padding(
            padding: const EdgeInsets.all(10),
            child: Container(
              alignment: Alignment.center,
              decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(10),
                  boxShadow: [
                    BoxShadow(
                      color: grey.withOpacity(0.3),
                      blurRadius: 5,
                      spreadRadius: 2,
                    )
                  ]),
              child: ClipRRect(
                borderRadius: BorderRadius.circular(10),
                child: Stack(children: [
                  FutureBuilder(
                      future: current.image,
                      builder: ((context, snapshot) {
                        if (!snapshot.hasData) {
                          return niceLookingSpinnerFuckYou();
                        } else if (snapshot.hasError) {
                          return const Text('error???');
                        }

                        var image = snapshot.requireData;

                        return Container(
                          width: size.width,
                          height: size.height,
                          decoration: BoxDecoration(
                              image: DecorationImage(
                                  image: image.image, fit: BoxFit.cover)),
                        );
                      })),
                  Container(
                    decoration: BoxDecoration(
                      gradient: LinearGradient(
                        colors: [
                          black.withOpacity(0.5),
                          black.withOpacity(0),
                        ],
                        end: Alignment.topCenter,
                        begin: Alignment.bottomCenter,
                      ),
                    ),
                    child: Padding(
                      padding: const EdgeInsets.all(20),
                      child: Column(
                        mainAxisAlignment: MainAxisAlignment.end,
                        children: [
                          Row(
                            children: [
                              Text(
                                "${current.firstName},",
                                style: const TextStyle(
                                  fontSize: 24,
                                  color: white,
                                  fontWeight: FontWeight.bold,
                                ),
                              ),
                              const SizedBox(
                                width: 10,
                              ),
                              Text(
                                '${current.age}',
                                style: const TextStyle(
                                  fontSize: 22,
                                  color: white,
                                ),
                              ),
                            ],
                          ),
                        ],
                      ),
                    ),
                  ),
                ]),
              ),
            ));
      },
      onStackFinished: () {
        setState(() {
          _cardContext = fillCards(user);
        });
      },
      itemChanged: (SwipeItem item, int index) {
        print("index: $index");
      },
      upSwipeAllowed: true,
      fillSpace: true,
    );
  }

  Widget getFooter(MatchEngine? engine) {
    var size = MediaQuery.of(context).size;
    return Container(
      width: size.width,
      height: size.height * 0.2 - 15,
      decoration: BoxDecoration(color: black),
      child: Padding(
        padding: const EdgeInsets.all(20),
        child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceEvenly,
            children: List.generate(item_icons.length, (index) {
              return ElevatedButton(
                style: ElevatedButton.styleFrom(
                  shape: CircleBorder(),
                  padding: EdgeInsets.all(12),
                ),
                child: Container(
                  width: item_icons[index]['size'],
                  height: item_icons[index]['size'],
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: white,
                    boxShadow: [
                      BoxShadow(
                        color: grey.withOpacity(0.1),
                        spreadRadius: 5,
                        blurRadius: 10,
                      ),
                    ],
                  ),
                  child: Center(
                    child: SvgPicture.asset(item_icons[index]['icon'],
                        width: item_icons[index]['icon_size']),
                  ),
                ),
                onPressed: () {
                  var cur = engine?.currentItem;
                  if (index == 0) {
                    cur?.nope();
                  } else if (index == 1) {
                    cur?.superLike();
                  } else {
                    // index == 2
                    cur?.like();
                  }
                },
              );
            })),
      ),
    );
  }
}

Future<CardContext> fillCards(User user) => fetchMatches(user).then((items) {
      var cards = items.map((i) => toSwipeItem(user, i)).toList();
      return CardContext(
          engine: MatchEngine(swipeItems: cards), matches: items);
    });

class CardContext {
  final MatchEngine engine;
  final List<Match> matches;

  CardContext({required this.engine, required this.matches});
}

SwipeItem toSwipeItem(User user, Match m) => SwipeItem(
    content: m,
    superlikeAction: () {
      decide(user, m.id, true);
    },
    likeAction: () {
      decide(user, m.id, true);
    },
    nopeAction: () {
      decide(user, m.id, false);
    });
