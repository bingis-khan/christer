import 'dart:io';
import 'package:flutter/material.dart';
//import 'package:flutter_svg/svg.dart';
import 'package:christer/theme/colors.dart';
//import 'package:image_picker/image_picker.dart';
//import 'package:path_provider/path_provider.dart';
import 'package:file_picker/file_picker.dart';

class PickPhotoScreen extends StatefulWidget {
  @override
  _PickPhotoScreenState createState() => _PickPhotoScreenState();
}

class _PickPhotoScreenState extends State<PickPhotoScreen> {
  File? _image = null;

  Future getImage() async {
    FilePickerResult? result = await FilePicker.platform.pickFiles(
      type: FileType.custom,
      allowedExtensions: ['jpg', 'png', 'jpeg'],
    );
    if (result == null) return;

    File file = File(result.files.single.path!);

    // State is pointless here.
    setState(() {
      this._image = file;
    });
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
          (_image == null)
              ? Image.asset("assets/images/profile.png",
                  height: 150, width: 150)
              : Image.file(File(_image!.path.toString()),
                  height: 150, width: 150),
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
