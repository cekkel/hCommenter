import { useMutation } from "@tanstack/react-query"
import { Button, Dialog, DialogTrigger, Form, Label, OverlayArrow, Popover, TextArea, TextField } from "react-aria-components"
import { postCommentsNewMutation } from "../../client/@tanstack/react-query.gen"

interface NewCommentProps {
  parentId?: number;
  author: string;
  convoUrl: string;
}

export const NewComment = (props: NewCommentProps) => {
  const createComment = useMutation({ ...postCommentsNewMutation() })

  const sendNewCommentRequest = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault()
    const formData = new FormData(e.currentTarget)
    const content = formData.get("ontent") as string

    createComment.mutate({
      body: {
        message: content,
        parent: props.parentId,
        author: props.author,
        convoUrl: props.convoUrl,
      },
    })
  }

  const restartForm = () => {
    createComment.reset();
  }

  return (
    <DialogTrigger>
      <Button className="btn-primary">
        Reply
      </Button>
      <Popover className="w-[32rem]">
        <OverlayArrow className="placement-bottom:rotate-180 stroke-gray-400 fill-white" >
          <svg width={12} height={12} viewBox="0 0 12 12">
            <path d="M0 0 L6 6 L12 0" />
          </svg>
        </OverlayArrow>
        <Dialog className="bg-white rounded-lg shadow-lg ring-1 ring-gray-400 p-6">
          <h2 className="text-2xl font-semibold mb-6 pb-2 border-b">
            New Comment
          </h2>
          {createComment.isPending && (
            <div className="text-gray-500">
              Creating comment...
            </div>
          )}

          {createComment.isError && (
            <div className="text-red-500 flex flex-col items-center gap-2">
              <p>
                An error occurred while trying to create your comment. Please try again.
              </p>
              <Button className="btn-secondary" onPress={restartForm}>
                Try again
              </Button>
            </div>
          )}

          {createComment.isSuccess && (
            <div className="text-green-500">
              Comment created successfully!
            </div>
          )}

          {createComment.isIdle && (
            <Form
              onSubmit={sendNewCommentRequest}
            >
              <TextField className="mb-4">
                <Label className="text-sm font-medium mb-1">
                  Content
                </Label>
                <TextArea
                  name="content"
                  className="input min-h-[120px]"
                  placeholder="Write your comment here..."
                />
              </TextField>
              <div className="mt-6 flex justify-end gap-3">
                <Button
                  onPress={close}
                  className="btn-secondary"
                >
                  Cancel
                </Button>
                <Button
                  type="submit"
                  className="btn-primary"
                >
                  Submit
                </Button>
              </div>
            </Form>
          )}
        </Dialog>
      </Popover>
    </DialogTrigger>
  )
}
