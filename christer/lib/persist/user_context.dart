import 'package:flutter/material.dart';

class UserContext extends InheritedWidget {
  final User user;

  const UserContext({super.key, required this.user, required super.child});

  static User of(BuildContext context) {
    final userContext =
        context.dependOnInheritedWidgetOfExactType<UserContext>();
    assert(userContext != null, 'No UserContext found - weird.');
    return userContext!.user;
  }

  @override
  bool updateShouldNotify(UserContext oldWidget) =>
      oldWidget.user.email != user.email ||
      oldWidget.user.password != user.password;
}

class User {
  final String email, password;

  const User({required this.email, required this.password});
}
