import 'package:christer/persist/persist.dart';
import 'package:flutter/material.dart';

class Match {
  final int id;
  final String firstName;
  final String lastName;
  final int age;
  final int height;
  final String race;
  final String description;
  final Future<Image> image;

  Match(
      {required this.id,
      required this.firstName,
      required this.lastName,
      required this.age,
      required this.height,
      required this.race,
      required this.description,
      required this.image});

  @override
  String toString() => firstName;

  factory Match.fromJson(Map<String, dynamic> json) => Match(
      id: json['userID'],
      firstName: json['firstName'] ?? 'Anonymous',
      lastName: json['lastName'] ?? 'Anonymous',
      age: json['age'] ?? -1,
      height: json['height'] ?? -1,
      race: json['race'] ?? '???',
      description: json['description'],
      image: fetchImage(json['userID']));
}
