import 'package:christer/pages/login.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:christer/pages/root_app.dart';
import 'package:provider/provider.dart';

import 'persist/persist.dart';

class UserPhoto extends ChangeNotifier {
  Future<Image> _image;

  UserPhoto({required Future<Image> image}) : _image = image;

  Future<Image> get image => _image;
  void set(Future<Image> image) {
    _image = image;
    notifyListeners();
  }
}

class OuterFuck extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    var userContext = context.watch<UserContext>();
    var child = MaterialApp(home: RootApp(), debugShowCheckedModeBanner: false);

    if (userContext.isLoggedIn()) {
      var user = userContext.user;
      return ChangeNotifierProvider(
          create: (context) => UserPhoto(
                image: fetchOwnImage(context.read<UserContext>().user),
              ),
          child: child);
    } else {
      return child;
    }
  }
}

void main() {
  runApp(
      ChangeNotifierProvider(create: (_) => UserContext(), child: OuterFuck()));
}
