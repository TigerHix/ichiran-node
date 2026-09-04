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
            ForEach(Array(path.tokens.enumerated()), id: \.element.id) { tokenIndex, token in
              Button {
                Task {
                  await model.tokenAction(
                    pathIndex: 0,
                    tokenIndex: tokenIndex,
                    entryIndex: token.entryIndex
                  )
                }
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
                  Image(systemName: "info.circle")
                }
              }
              .buttonStyle(.plain)
            }
          }
        }

        if let details = model.tokenDetails {
          Section("Canonical TokenDetails") {
            LabeledContent("Text", value: details.text)
            LabeledContent("Reading", value: details.reading)
            ForEach(Array(details.meanings.enumerated()), id: \.offset) { _, meaning in
              VStack(alignment: .leading, spacing: 3) {
                Text(meaning.gloss)
                Text(meaning.pos.joined(separator: ", "))
                  .font(.caption)
                  .foregroundStyle(.secondary)
              }
            }
            ForEach(Array(details.components.enumerated()), id: \.offset) { _, component in
              LabeledContent("Component", value: "\(component.text)  \(component.reading)")
              if let suffix = component.suffix {
                LabeledContent("Suffix", value: suffix)
              }
            }
            ForEach(Array(details.alternatives.enumerated()), id: \.offset) { _, alternative in
              LabeledContent("Alternative", value: "\(alternative.text)  \(alternative.reading)")
            }
            if let conjugation = details.conjugations.first {
              LabeledContent("Conjugation root", value: conjugation.root?.text ?? "—")
              if let via = conjugation.via.first?.root?.text {
                LabeledContent("Via", value: via)
              }
            }
            if let counter = details.counter {
              LabeledContent("Counter", value: counter.value)
            }
            if details.entity {
              LabeledContent("Entity", value: "true")
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
