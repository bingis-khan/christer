import 'package:flutter/material.dart';

class Match {
  final int id;
  final String firstName;
  final String lastName;
  final int age;
  final int height;
  final String race;
  final String description;
  final Image image;

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
      id: json['id'],
      firstName: json['firstName'],
      lastName: json['lastName'],
      age: json['age'],
      height: json['height'],
      race: json['race'],
      description: json['description'],
      image: Image.network(
          'https://cdn.cloudflare.steamstatic.com/steam/apps/364190/ss_7e0e4b0cb2cf6d266b9814e4d51e06cc06d0a91a.1920x1080.jpg?t=1572321559') //Image.network('$host/pic/${json['imageID'] as int}', headers: headers),
      );
}
