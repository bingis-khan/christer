import 'package:flutter/material.dart';
import 'package:christer/theme/colors.dart';
import 'package:christer/data/chats_json.dart';

class ChatPage extends StatefulWidget {
  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> {
  @override
  Widget build(BuildContext context){
    return Scaffold(
      backgroundColor: black,
      body: getBody(),
    );
  }

  Widget getBody() {
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
            ),
          ),
        ),
        SizedBox(
          height: 5,
        ),
        Padding(
          padding: const EdgeInsets.all(10),
          child: Column(
            children: List.generate(chats_json.length, (index) {
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
                          image: AssetImage(chats_json[index]['img']),
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
                          Text(chats_json[index]['name'], 
                            style: const TextStyle(
                              fontSize: 22,
                              color: white,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                          
                          Text(chats_json[index]['messages'].length > 0 ? 
                          (
                            (chats_json[index]['messages'][chats_json[index]['messages'].length - 1]['isYours'] ? 'You: ' : '') + 
                            chats_json[index]['messages'][chats_json[index]['messages'].length - 1]['msg'] + ' - ' + 
                            chats_json[index]['messages'][chats_json[index]['messages'].length - 1]['created_at']
                          ) :
                          "Say hi to your new match!", 
                            style: const TextStyle(
                              fontSize: 15,
                              color: white,
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