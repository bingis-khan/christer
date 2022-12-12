import 'dart:io';
import 'package:christer/persist/persist.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
//import 'package:flutter_svg/svg.dart';
import 'package:christer/theme/colors.dart';
//import 'package:image_picker/image_picker.dart';
//import 'package:path_provider/path_provider.dart';
import 'package:file_picker/file_picker.dart';
import 'package:provider/provider.dart';

import '../main.dart';
import 'account_page.dart';

class PickPhotoScreen extends StatefulWidget {
  @override
  State<PickPhotoScreen> createState() => _PickPhotoScreenState();
}

const errorSnackBar = SnackBar(content: Text('Too many or no files selected.'));

class _PickPhotoScreenState extends State<PickPhotoScreen> {
  Future getImage() async {
    FilePickerResult? result = await FilePicker.platform.pickFiles(
      type: FileType.custom,
      allowedExtensions: ['jpg', 'png', 'jpeg'],
    );

    // Check if the user has exited already (we don't care then).
    if (!mounted) return;

    if (result == null) return;

    if (!result.isSinglePick) {
      ScaffoldMessenger.of(context).showSnackBar(errorSnackBar);
      return;
    }

    var path = result.files.single.path!;
    var user = context.read<UserContext>().user;
    var error = await uploadImage(user, path);
    if (error == null) {
      if (!mounted) return;
      context.read<UserPhoto>().set(fetchOwnImage(user));
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: white,
      appBar: AppBar(
        title: Text("Pick a Photo"),
        backgroundColor: black,
      ),
      body: Center(
          child: Column(
        children: [
          SizedBox(height: 10),
          FutureBuilder(
            future: context.watch<UserPhoto>().image,
            builder: (context, snapshot) {
              if (!snapshot.hasData) {
                return const CircularProgressIndicator();
              }

              var image = snapshot.requireData;
              var size = MediaQuery.of(context).size;
              return Container(
                width: size.width / 2,
                height: size.height / 2,
                decoration: BoxDecoration(
                    image:
                        DecorationImage(image: image.image, fit: BoxFit.cover)),
              );
            },
          ),
          SizedBox(height: 40),
          CustomButton(
            title: 'Pick from Gallery',
            icon: Icons.image_outlined,
            onClick: getImage,
          ),
        ],
      )),
    );
  }

  Widget CustomButton(
      {required String title,
      required IconData icon,
      required VoidCallback onClick}) {
    return Container(
      width: 200,
      child: ElevatedButton(
        onPressed: onClick,
        child: Row(
          children: [
            Icon(icon),
            SizedBox(width: 20),
            Text(title),
          ],
        ),
      ),
    );
  }
}
