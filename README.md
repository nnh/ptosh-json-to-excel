# JSON to Excel Converter

このプログラムは R 言語で書かれており、R Studio での実行を前提としています。入力として JSON ファイルを処理し、出力として.xlsx ファイルを生成します。

## 使用手順

1. リポジトリの [Code] ボタンをクリックし、[Download ZIP] を選択して ZIP ファイルをダウンロードします。

2. ダウンロードした ZIP ファイルを適当なディレクトリに「すべて展開」します。 初期状態では`input`フォルダ、`output`フォルダに`.gitkeep`という隠しファイルが存在します。処理に影響はありませんが、不要であれば削除して下さい。

3. R Studio を開きます。

4. R Studio のメニューから [File] > [New Project...] を選択します。

5. [New Project] ダイアログが表示されたら、[Existing Directory] を選択します。

6. [Project working directory] > [Browse...] を選択します。 先ほど「すべて展開」したフォルダを選択し、[ptosh-json-to-excel-master] > [ptosh-json-to-excel-master] を開いて[Open] を選択します。

7. [Create Project] ボタンをクリックしてプロジェクトを開きます。

8. JSON ファイルが入ったフォルダを `input` ディレクトリに格納してください。ファイルの拡張子は必ず `.json` である必要があります。 JSON フォルダが入ったフォルダの名前を「試験名略称」として扱います。

9. [File] > [Open File...]から、`prg/json_to_excel.R` ファイルを開いてください。
10. `source` をクリックし、 画面左下の console に「処理が終了しました。」と出るまでお待ちください。 処理の途中で「パブリックネットワークとプライベートネットワークにこのアプリのアクセスを許可しますか？」という Windows セキュリティのポップアップが出た場合は、「キャンセル」をクリックして下さい。

11. 処理された JSON ファイルに対応する.xlsx ファイルが `output/output_処理年月/` ディレクトリに出力されます。`output_処理年月`のフォルダは自動で作成されます。

12. 上記ディレクトリ内には、プログラムが生成した `list` フォルダがあり、その中には入力 JSON ファイルの内容を集計した `試験名略称 eCRF Spec YYYYMMDD.xlsx` が出力されます。

## ディレクトリ構造

```
ptosh-json-to-excel
├── LICENSE
├── README.md
├── input
│   └── trial_name
│       ├── aaa.json
│       └── zzz.json
├── output
│   └── output_YYYYMMDDHHMMSS
│       ├── aaa.xlsx
│       ├── list
│       │   └── trial_name eCRF Spec YYYYMMDD.xlsx
│       └── zzz.xlsx
├── prg
│   ├── functions
│   └── json_to_excel.R
├── temp
├── testscript
└── tools
```

## 注意事項

- プログラムが前提とするディレクトリ構造が整っていることを確認してください。
