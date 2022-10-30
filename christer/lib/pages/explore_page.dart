import 'package:flutter/material.dart';
//import 'package:flutter_svg/svg.dart';

import 'package:christer/data/explore_json.dart';
import 'package:christer/data/icons.dart';
import 'package:christer/theme/colors.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:flutter_tindercard/flutter_tindercard.dart';

class ExplorePage extends StatefulWidget {
  @override
  _ExplorePageState createState() => _ExplorePageState();
}

class _ExplorePageState extends State<ExplorePage> {
  List itemsTemp = [];
  int itemLength = 0;
  @override 
  void initState(){
    super.initState();
    setState(() {
      itemsTemp = explore_json;
      itemLength = explore_json.length;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: white,
      body: getBody(),
      //bottomSheet: getFooter(),
    );
  }

  
  Widget getBody(){
    var size = MediaQuery.of(context).size;
    return Padding(
      padding: const EdgeInsets.only(bottom: 1),
      child: Container(
        height: size.height,
        child: TinderSwapCard(
          totalNum: itemLength,
          maxWidth: size.width,
          maxHeight: size.height * 0.99,
          minWidth: size.width * 0.75,
          minHeight: size.height * 0.8,
          cardBuilder: (context, index) => Container(
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
                      image: DecorationImage(image: AssetImage(explore_json[index]['img']), fit: BoxFit.cover),
                    ),
                  ),
                  Container(
                    width: size.width,
                    height: size.height,
                    decoration: BoxDecoration(
                      gradient: LinearGradient(colors: [
                          black.withOpacity(0.25),
                          black.withOpacity(0),
                        ],
                        end: Alignment.topCenter,
                        begin: Alignment.bottomCenter,

                      ),
                    ),
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      mainAxisAlignment: MainAxisAlignment.end,
                      children: [
                        Padding(padding: const EdgeInsets.only(left: 15, bottom: 15),
                          child: Row(children: [
                            Container(
                              width: size.width * 0.72,
                              child: Column(
                                children: [
                                  Row(
                                    children: [
                                      Text(itemsTemp[index]['name'], style: TextStyle(
                                          fontSize: 24,
                                          color: white,
                                          fontWeight: FontWeight.bold,
                                        ),
                                      ),
                                      SizedBox(width: 10, ),
                                      Text(itemsTemp[index]['age'], style: TextStyle(
                                          fontSize: 22,
                                          color: white,
                                        ),
                                      ),
                                    ],
                                  ),
                                ],
                              ),
                            ),
                          ],)
                        ),
                        
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }
  

  
  Widget getFooter() {
    var size = MediaQuery.of(context).size;
    return Container(
      width: size.width,
      height: 120,
      decoration: BoxDecoration(
        color: white
      ),
      child: Padding(
        padding: const EdgeInsets.only(left: 20, right: 20, bottom: 20),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: List.generate(item_icons.length, (index) {
            return Container(
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
            );
          })
        ),
      ),
    );
  }
  
}