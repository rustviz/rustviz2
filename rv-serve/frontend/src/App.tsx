import React, { useEffect, useState } from 'react';
import './index.css';
import { extensions } from './setup';
import { EditorState } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import axios from 'axios';
import ErrorCard from './ErrorCard';

declare function helpers(param: string): void;

const defaultExample: string = `
fn main () {
    let mut x = 7;
    let mut z = 6;
    let mut a = & mut x;
    let mut c = & mut z;
    let mut b = & mut a;
    b = & mut c;
    println!("x {}", *a);
    println!("z {}", **b);
}
`.trim();

class Editor {
  private view: EditorView;

  public constructor (
    editorContainer: HTMLElement,
    code: string = defaultExample,
  ) {
    let initial_state = EditorState.create({
      doc: code,
      extensions: extensions
    });
    
    this.view = new EditorView({
      state: initial_state,
      parent: editorContainer
    });
  }

  public getCurrentCode(): string {
    return this.view.state.doc.toString();
  }
}

const App = () => {
  const [isLoading, setIsLoading] = useState(false);
  const [isErr, setErr] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [editor, setEditor] = useState<Editor | null>(null);
  const [code_svg, setCodeSvg] = useState<string | null>(null);
  const [timeline_svg, setTimelineSvg] = useState<string | null>(null);

  useEffect(() => {
    const editorElement = document.getElementById('editor')!;
    if (editorElement) {
      const newEditor = new Editor(editorElement);
      setEditor(newEditor);
    }
  }, []);

  const handleClick = async () => {
    if (!editor) return;

    setIsLoading(true);
    const code = editor.getCurrentCode();

    try {
      const response = await axios.post('/submit-code', { code });

      if (response.status === 200) {
        setCodeSvg(response.data.code_panel);
        setTimelineSvg(response.data.timeline_panel);
        setErr(false);
      } else {
        console.error('Error:', response.statusText);
        setError(response.data);
        setErr(true);
      }
    } catch (error) {
      if (axios.isAxiosError(error) && error.response) {
        setError(error.response.data); // Extract and set error message
      } else {
        setError('An unexpected error occurred');
      }
      console.error('An error occurred:', error);
      setErr(true);
    } finally {
      setIsLoading(false);
    }
  };

  useEffect(() => {
    handleClick(); // Call handleClick when the component mounts
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [editor]);


  return (
    <div id="page-wrapper" className="page-wrapper">
      <button className="cm-button large-button" id="gen-button" onClick={handleClick} disabled={isLoading}>
        {isLoading ? <span className="loader"></span> : 'Generate Visualization'}
      </button>
      {isErr && error ? <ErrorCard err_string={error} /> :
        <div className="page">
          <div className="flex-container vis_block" style={{ marginLeft: '50px' }}>
            <div
              className="ex2 code_panel"
              dangerouslySetInnerHTML={{ __html: code_svg ?? "" }}
            />
            <div
              className="ex2 tl_panel"
              style={{ width: 'auto' }}
              dangerouslySetInnerHTML={{ __html: timeline_svg ?? "" }}
              onMouseEnter={() => helpers('ex2')}
            />
          </div>
        </div>
      }
    </div>
  );
};

export default App;