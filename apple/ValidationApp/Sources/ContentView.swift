import IchiranSwift
import SwiftUI

struct ContentView: View {
  @StateObject private var model = ValidationModel()

  var body: some View {
    NavigationStack {
      Form {
        Section("Pack") {
          Text(model.status)
            .font(.footnote)
            .foregroundStyle(.secondary)
          TextField("https://…/manifest.json", text: $model.remoteManifest)
            .textInputAutocapitalization(.never)
            .keyboardType(.URL)
          Button("Install remote release") {
            Task { await model.installRemote() }
          }
        }

        Section("Japanese text") {
          TextEditor(text: $model.text)
            .frame(minHeight: 90)
          Button("Analyze") {
            Task { await model.analyzeAction() }
          }
          .buttonStyle(.borderedProminent)
        }

        if !model.romanized.isEmpty {
          Section("Romanization") {
            Text(model.romanized)
              .textSelection(.enabled)
          }
        }

        if let path = model.analysis?.paths.first {
          Section("Best path") {
            ForEach(path.tokens) { token in
              Button {
                guard let index = token.entryIndex else { return }
                Task { await model.entryAction(index) }
              } label: {
                HStack {
                  VStack(alignment: .leading) {
                    Text(token.text)
                      .font(.headline)
                    Text(token.reading.isEmpty ? token.route.rawValue : token.reading)
                      .font(.caption)
                      .foregroundStyle(.secondary)
                  }
                  Spacer()
                  if token.entryIndex != nil {
                    Image(systemName: "book")
                  }
                }
              }
              .buttonStyle(.plain)
            }
          }
        }

        if let entry = model.entry {
          Section("Dictionary #\(entry.seq)") {
            Text(entry.forms.map(\.text).joined(separator: "・"))
              .font(.headline)
            ForEach(entry.senses, id: \.ord) { sense in
              Text(sense.glosses.map(\.text).joined(separator: "; "))
            }
          }
        }

        if let message = model.errorMessage {
          Section("Action required") {
            Text(message)
              .foregroundStyle(.red)
              .textSelection(.enabled)
          }
        }
      }
      .navigationTitle("Ichiran Validation")
      .disabled(model.isBusy)
      .task { await model.start() }
    }
  }
}
