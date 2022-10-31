import 'package:christer/pages/explore_page.dart';
import 'package:flutter/material.dart';
import 'package:flutter_svg/svg.dart';
import 'package:christer/theme/colors.dart';
//import 'package:christer/pages/account_page.dart';
import 'package:christer/pages/chat_page.dart';
import 'package:christer/pages/explore_page.dart';
//import 'package:christer/pages/likes_page.dart';

class RootApp extends StatefulWidget {
    @override
    _RootAppState createState() => _RootAppState();
}

class _RootAppState extends State<RootApp> {
    int pageIndex = 0;

    @override
    Widget build(BuildContext context){
        var items = [
            pageIndex == 0 ? "assets/images/explore_active_icon.svg" : "assets/images/explore_icon.svg",
            pageIndex == 1 ? "assets/images/likes_active_icon.svg" : "assets/images/likes_icon.svg",
            pageIndex == 2 ? "assets/images/chat_active_icon.svg" : "assets/images/chat_icon.svg",
            pageIndex == 3 ? "assets/images/account_active_icon.svg" : "assets/images/account_icon.svg",
        ];

        return Scaffold(
            backgroundColor: white,
            appBar: AppBar(
                backgroundColor: white,
                elevation: 0,
                title: Padding(
                    padding: const EdgeInsets.only(left: 10, right: 10),
                    child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: List.generate(items.length, (index){
                            return IconButton(
                                onPressed: () {
                                    setState((){
                                        pageIndex = index;
                                    });
                                },
                                icon: SvgPicture.asset(items[index]),
                            );
                        }),
                    ),
                ),
            ),
            body: getBody(),
        );
    }

    Widget getBody() {
        return IndexedStack(
            index: pageIndex,
            children: [
                ExplorePage(),
                ExplorePage(),
                ChatPage(),
                ExplorePage(),
            ],
        );
    }
    /*
    Widget getAppBar() {
        
        return AppBar(
            backgroundColor: white,
            elevation: 0,
            title: Row(
                children: List.generate(items.length, (index){
                    return IconButton(
                        onPressed: () {
                            setState((){
                                pageIndex = index;
                            });
                        },
                        icon: SvgPicture.asset(items[index]),
                    );
                }),
            ),
        );
    }*/
}