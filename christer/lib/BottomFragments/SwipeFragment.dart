import 'package:flutter/material.dart';

class SwipeFragment extends StatefulWidget {
  const SwipeFragment({Key? key}) : super(key: key);

  @override
  _SwipeFragmentState createState() => _SwipeFragmentState();
}

class _SwipeFragmentState extends State<SwipeFragment> {
  @override
  Widget build(BuildContext context) {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Center(
          child: Text("This is Swipe Screen", style: TextStyle(fontSize: 24),),
        )
      ],
    );
  }
}