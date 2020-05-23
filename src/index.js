import * as firebase from "firebase/app";
import "firebase/auth";
import "firebase/firestore";

import { Elm } from "./Main.elm";

const FIREBASE_CONFIG = {
  apiKey: "AIzaSyATojF6BXhvtRg_5Lkr5HIeygMKyk7a2g8",
  authDomain: "kaertchen-2020.firebaseapp.com",
  databaseURL: "https://kaertchen-2020.firebaseio.com",
  projectId: "kaertchen-2020",
  storageBucket: "kaertchen-2020.appspot.com",
  messagingSenderId: "1051493311654",
  appId: "1:1051493311654:web:55442f3bee175d031b0067",
  measurementId: "G-XKMMMG3ZC1",
};

firebase.initializeApp(FIREBASE_CONFIG);

const provider = new firebase.auth.GoogleAuthProvider();

const db = firebase.firestore();

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

app.ports.signIn.subscribe(() => {
  firebase
    .auth()
    .signInWithRedirect(provider)
    .catch(() => app.ports.signInError.send(null));
});

app.ports.signOut.subscribe(() => {
  firebase.auth().signOut();
});

let unsubscribeFromCollectionUpdates;

firebase.auth().onAuthStateChanged((user) => {
  if (user === null) {
    unsubscribeFromCollectionUpdates?.();
    app.ports.loggedOutUser.send(null);
    return;
  }

  app.ports.signInInfo.send({
    email: user.email,
    name: user.displayName,
    uid: user.uid,
  });

  unsubscribeFromCollectionUpdates = db
    .collection(`users/${user.uid}/cards`)
    .onSnapshot(
      ({ docs }) => {
        app.ports.receiveCards.send({
          cards: docs.map((doc) => ({
            ...doc.data(),
            id: doc.id,
          })),
        });
      },
      () => app.ports.receiveCardsError.send(null)
    );
});

app.ports.saveEditedCard.subscribe((data) => {
  db.collection(`users/${data.uid}/cards`)
    .doc(data.id)
    .update({
      phrase: data.phrase,
      translation: data.translation,
      title: data.title,
    })
    .catch(() => app.ports.saveEditedCardError.send(null));
});

app.ports.addNewCard.subscribe((data) => {
  db.collection(`users/${data.uid}/cards`)
    .add({
      phrase: data.phrase,
      translation: data.translation,
      title: data.title,
    })
    .catch(() => app.ports.addNewCardError.send(null));
});
