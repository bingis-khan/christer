import 'package:christer/BottomFragments/SwipeFragment.dart';
import 'package:christer/BottomFragments/ChatFragment.dart';
import 'package:christer/BottomFragments/ProfileFragment.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const MyHomePage(title: 'Flutter Demo Home Page'),
      //debugShowCheckedModeBanner: false,
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key, required this.title});

  final String title;

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {

  int selectedIndex = 0;

  final widgetTitle = ["Swipe", "Chat", "Profile"];

  //list of widgets to call ontap
  final widgetOptions = [
    new SwipeFragment(),
    new ChatFragment(),
    new ProfileFragment(),
  ];

  void onItemTapped(int index) {
    setState(() {
      selectedIndex = index;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widgetTitle.elementAt(selectedIndex)),
      ),
      bottomNavigationBar: BottomNavigationBar(
      items: <BottomNavigationBarItem>[
        BottomNavigationBarItem(
            icon: Icon(Icons.favorite, color: Colors.blue,), label: "Swipe"),
        BottomNavigationBarItem(
            icon: Icon(Icons.messenger, color: Colors.blue,), label: "Chat"),
        BottomNavigationBarItem(
            icon: Icon(Icons.account_circle_sharp, color: Colors.blue,), label: "Profile"),
      ],
      currentIndex: selectedIndex,
      onTap: onItemTapped,

      selectedLabelStyle: TextStyle(color: Colors.red, fontSize: 20),
      fixedColor: Colors.red,
      unselectedFontSize: 16,
      unselectedItemColor: Colors.blue,
      ),
      body: Center(
        child: widgetOptions.elementAt(selectedIndex),
      ),
    );
  }
}
