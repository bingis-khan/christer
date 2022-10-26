import 'package:flutter/material.dart';

class ChatFragment extends StatefulWidget {
  const ChatFragment({Key? key}) : super(key: key);

  @override
  _ChatFragmentState createState() => _ChatFragmentState();
}

class _ChatFragmentState extends State<ChatFragment> {
  @override
  Widget build(BuildContext context) {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Center(
          child: Text("This is Chat Screen", style: TextStyle(fontSize: 24),),
        )
      ],
    );
  }
}