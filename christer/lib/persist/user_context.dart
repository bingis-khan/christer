import 'package:flutter/material.dart';

class UserContext extends ChangeNotifier {
  User? _user;

  UserContext();

  User get user => _user!;

  bool isLoggedIn() => _user != null;

  void login(User user) {
    _user = user;
    notifyListeners();
  }

  void logout() {
    _user = null;
    notifyListeners();
  }
}

class User {
  final String email, password;

  const User({required this.email, required this.password});
}
