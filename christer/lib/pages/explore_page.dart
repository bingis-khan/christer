import 'package:flutter/material.dart';
import 'package:christer/data/explore_json.dart';
import 'package:christer/data/icons.dart';
import 'package:christer/theme/colors.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:swipe_cards/draggable_card.dart';
import 'package:swipe_cards/swipe_cards.dart';
import 'package:christer/model/content.dart';

class ExplorePage extends StatefulWidget {
  @override
  _ExplorePageState createState() => _ExplorePageState();
}

class _ExplorePageState extends State<ExplorePage> {
  List<SwipeItem> _swipeItems = <SwipeItem>[];
  MatchEngine? _matchEngine;
  GlobalKey<ScaffoldState> _scaffoldKey = GlobalKey();

  @override 
  void initState(){
    for(int i=0; i < explore_json.length; i++){
      _swipeItems.add(SwipeItem(
        content: Content(
          name: explore_json[i]['name'],
          image_path: explore_json[i]['img'],
          age: explore_json[i]['age'],
          interests: explore_json[i]['likes']
        ),
        likeAction: () {
            print("Like action!");
          },
          nopeAction: () {
            print("Nope action!");
          },
          superlikeAction: () {
            print("Superlike action!");
          },
          onSlideUpdate: (SlideRegion? region) async {}
        )
      );
    }

    _matchEngine = MatchEngine(swipeItems: _swipeItems);

    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: black,
      body: getBody(),
      bottomSheet: getFooter(),
    );
  }

  Widget getBody(){
    var size = MediaQuery.of(context).size;
    return Padding(
      padding: const EdgeInsets.only(bottom: 1),
      child: Container(
        height: size.height * 0.75,
        child: SwipeCards(
          matchEngine: _matchEngine!,
          itemBuilder: (BuildContext context, int index) {
            return Padding(
              padding: const EdgeInsets.all(10),
              child: Container(
                alignment: Alignment.center,
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(10),
                  boxShadow: [BoxShadow(
                    color: grey.withOpacity(0.3),
                    blurRadius: 5,
                    spreadRadius: 2,
                  )]
                ),
                child: ClipRRect(
                  borderRadius: BorderRadius.circular(10),
                  child: Stack(
                    children: [
                      Container(
                        width: size.width,
                        height: size.height,
                        decoration: BoxDecoration(
                          image: DecorationImage(image: AssetImage(_swipeItems[index].content.image_path), fit: BoxFit.cover),
                        ),  
                      ),
                      Container(
                        decoration: BoxDecoration(
                          gradient: LinearGradient(colors: [
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
                                  Text(_swipeItems[index].content.name + ",", style: const TextStyle(
                                      fontSize: 24,
                                      color: white,
                                      fontWeight: FontWeight.bold,
                                    ),
                                  ),
                                  const SizedBox(width: 10, ),
                                  Text(_swipeItems[index].content.age, style: const TextStyle(
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
                    ]
                  ),
                ),
              )
            );
          },
          
          onStackFinished: () { // DEBUG - LOOP
            for(int i=0; i < explore_json.length; i++){
              _swipeItems.add(SwipeItem(
                content: Content(
                  name: explore_json[i]['name'],
                  image_path: explore_json[i]['img'],
                  age: explore_json[i]['age'],
                  interests: explore_json[i]['likes']
                ),
                likeAction: () {
                  // DEBUG
                  print("Like action!");
                },
                nopeAction: () {
                  // DEBUG
                  print("Nope action!");
                },
                superlikeAction: () {
                  // DEBUG
                  print("Superlike action!");
                },
                onSlideUpdate: (SlideRegion? region) async {
                }
                )
              );
            }
          },

          itemChanged: (SwipeItem item, int index) {
                print("index: $index");
          },
          upSwipeAllowed: true,
          fillSpace: true,
        ),
        
      ),
    );
  }
  
  Widget getFooter() {
    var size = MediaQuery.of(context).size;
    return Container(
      width: size.width,
      height: size.height * 0.2 - 15,
      decoration: BoxDecoration(
        color: black
      ),
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
                    width: item_icons[index]['icon_size']
                  ),
                ),
              ),
              onPressed: (){
                if(index == 0){
                  _matchEngine!.currentItem?.nope();
                }
                else if(index == 1){
                  _matchEngine!.currentItem?.superLike();
                } 
                else{ // index == 2
                  _matchEngine!.currentItem?.like();
                }
              },
            );
          })
        ),
      ),
    );
  }
}