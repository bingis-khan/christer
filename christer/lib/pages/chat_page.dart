import 'package:flutter/material.dart';
import 'package:christer/theme/colors.dart';
import 'package:christer/data/chats_json.dart';

class ChatPage extends StatefulWidget {
  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> {
  final myController = TextEditingController();
  List chats = chats_json;

  void _updateChats(String name){
    final filtered_chats = chats_json.where((element) {
      return element['name'].toLowerCase().contains(name.toLowerCase());
    }).toList();
    setState(() {
      chats = filtered_chats;
    });
  }

  @override
  Widget build(BuildContext context){
    return Scaffold(
      backgroundColor: black,
      body: getBody(),
    );
  }

  @override
  void dispose() {
    myController.dispose();
    super.dispose();
  }

  Widget getBody() {
    var size = MediaQuery.of(context).size;
    const MAX_LENGTH = 20;
    return ListView(
      children: [
        Padding(
          padding: const EdgeInsets.all(8),
          child: Container(
            height: 48,
            decoration: BoxDecoration(
                color: white.withOpacity(0.5),
                borderRadius: BorderRadius.circular(24)),
            child: TextField(
              cursorColor: black.withOpacity(0.5),
              decoration: InputDecoration(
                  border: InputBorder.none,
                  prefixIcon: Icon(
                    Icons.search,
                    color: black.withOpacity(0.5),
                  ),
                  hintText: "Search"),
                  controller: myController,
                  onChanged: _updateChats,
            ),
          ),
        ),
        const SizedBox(
          height: 5,
        ),
        Padding(
          padding: const EdgeInsets.all(10),
          child: Column(
            children: List.generate(chats.length, (index) {
              return Padding(
                padding: EdgeInsets.all(10),
                child: Row(
                  children: [
                    Container(
                      width: 70,
                      height: 70,
                      decoration: BoxDecoration(
                        shape: BoxShape.circle,
                        image: DecorationImage(
                          image: AssetImage(chats[index]['img']),
                          fit: BoxFit.cover,
                        )
                      ),
                    ),
                    SizedBox(
                      width: 20,
                    ),
                    Padding(
                      padding: EdgeInsets.all(10),
                      child: Column(
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: [
                          Text(chats[index]['name'], 
                            style: const TextStyle(
                              fontSize: 22,
                              color: white,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                          
                          SizedBox(
                            width: size.width - 150,
                            child: Text(chats[index]['messages'].length > 0 ? 
                            (
                              (chats[index]['messages'][chats[index]['messages'].length - 1]['isYours'] ? 'You: ' : '') + 
                              (
                                chats[index]['messages'][chats[index]['messages'].length - 1]['msg'].length < MAX_LENGTH ? 
                                chats[index]['messages'][chats[index]['messages'].length - 1]['msg'] : 
                                chats[index]['messages'][chats[index]['messages'].length - 1]['msg'].substring(0, MAX_LENGTH) + ' ... '
                              ) + ' - ' + 
                              chats[index]['messages'][chats[index]['messages'].length - 1]['created_at']
                            ) :
                            "Say hi to your new match!", 
                              style: const TextStyle(
                                fontSize: 15,
                                color: white,
                              ),
                            ),
                          ),                      
                        ],
                      ),
                    ),
                  ],
                ),
              );
            }
            ),
          ), 
        ),
      ],
    );
  }
}