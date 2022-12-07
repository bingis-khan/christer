// ignore_for_file: unnecessary_brace_in_string_interps

import 'dart:convert';

import 'package:christer/data/default_params.dart';
import 'package:christer/pages/root_app.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:flutter_login/flutter_login.dart';
import 'package:http/http.dart' as http;
//import 'package:flutter_session/flutter_session.dart';

class LoginScreen extends StatefulWidget {
  const LoginScreen({super.key});

  @override
  State<LoginScreen> createState() => _LoginScreenState();
}

class _LoginScreenState extends State<LoginScreen> {
  late String email, password;

  Duration get loginTime => Duration(milliseconds: 2250);

  Future<String?> _authUser(LoginData data) async {
    debugPrint('Name: ${data.name}, Password: ${data.password}');
    // return tryLogin(data.name, data.password).then((correct) {
    //   if (!correct) {
    //     return "Invalid name or password";
    //   }

    //   //var session = FlutterSession();
    //   return null;
    // });

    if (data.name == 'bob@bob.bob' && data.password == 'bob') {
      setState(() {
        email = data.name;
        password = data.password;
      });
      return null;
    } else {
      return "Invalid name or password.";
    }
  }

  Future<String?> _signupUser(SignupData data) {
    debugPrint('Signup Name: ${data.name}, Password: ${data.password}');
    return Future.delayed(loginTime).then((_) {
      return null;
    });
  }

  Future<String> _recoverPassword(String name) {
    debugPrint('Name: $name');
    return Future<String>.value("Not supported");
  }

  @override
  Widget build(BuildContext context) {
    return FlutterLogin(
      title: 'CHRISTER',
      logo: const AssetImage('assets/images/girls/img_11.jpeg'),
      onLogin: _authUser,
      onSignup: _signupUser,
      onSubmitAnimationCompleted: () {
        Navigator.of(context).pushReplacement(MaterialPageRoute(
          builder: (context) => UserContext(
              user: User(email: email, password: password), child: RootApp()),
        ));
      },
      onRecoverPassword: _recoverPassword,
    );
  }
}

Map<String, String> authHeaders(String email, String password) => {
      'Authorization': 'Basic ${base64.encode(utf8.encode('$email:$password'))}'
    };

Future<bool> tryLogin(String email, String password) async {
  final res = await http.get(Uri.parse('$host/current'),
      headers: authHeaders(email, password));

  if (res.statusCode == 401) {
    return false;
  } else if (res.statusCode == 200) {
    return true;
  } else {
    throw Exception('Cringe');
  }
}
